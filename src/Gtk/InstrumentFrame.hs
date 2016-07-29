{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.InstrumentFrame
    (
    InstrumentPage
    ,instrumentPageNew
    ,instrumentPageGetMainBox
    ,instrumentPageInsert
    ,instrumentPageSetInstrumentFile
    ,instrumentPageWriteInstrumentFile
    ,instrumentPageReset
    ,instrumentPageSetInstrumentName
    )
where

import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class (liftIO)
--import Control.Exception

import Prelude as P

import Graphics.UI.Gtk

import Gtk.Colors
import Gtk.Utils


import Data.Text as T
import Data.Text.IO as T
--import qualified Data.Text.Lazy as TL
--import Data.Text.Format
--import qualified Data.ByteString.Lazy as B

import Data.Types
import Data.IORef
import Data.Checkers
import Data.Drumgizmo
import Data.Export
import Data.Maybe
import Data.Char (isSpace)
import Data.Either
import Data.List as L (intercalate)
import qualified Data.Vector as V

import Data.DrumDrops.Utils

import System.FilePath

import Gtk.InstrumentPageBuilder

import Network.URI (unEscapeString)

import Sound.File.Sndfile (getFileInfo, Info(..))




data InstrumentPage = InstrumentPage {
    guiInstMainBox :: Box,
    guiMainWindow :: Window,
    guiIPNotebook :: Notebook,
    guiIPEntryBaseDir :: Entry,
    guiIPEntrySamplesDir :: Entry,
    guiIPInstrumentPages :: IORef (V.Vector InstrumentPage),
    guiInstHitView :: TreeView,
    guiInstHitViewModel :: ListStore HitSample,
    guiRendererHP :: CellRendererText,
    guiRendererHPName :: CellRendererText,
    guiRendererChan :: CellRendererText,
    guiRendererFileChan :: CellRendererText,
    guiInstSamplesView :: TreeView,
    guiInstSamplesViewModel :: ListStore AudioFile,
    guiInstFile :: IORef (Maybe InstrumentFile),
    guiEntryVersion :: Entry,
    guiEntryName :: Entry,
    guiEntryType :: Entry,
    guiAudioSamplesMenu :: Menu,
    guiIPParserCombo :: ComboBox,
    guiHitSamplesMenu :: Menu
    }




instrumentPageNew :: Window -> Notebook -> Entry -> Entry -> ComboBox -> IORef (V.Vector InstrumentPage) -> IO InstrumentPage
instrumentPageNew parentWindow notebook basedir samplesDir combo ioref = do
    -- Create the builder, and load the UI file
    builder <- builderNew

    builderAddFromFile builder "InstrumentPage.glade"
    --builderAddFromString builder builderFileAsString

    -- Retrieve some objects from the UI
    mainBox <- builderGetObject builder castToBox ("mainBox" :: Text)
    treeviewHit <- builderGetObject builder castToTreeView ("treeviewHit" :: Text)
    treeviewSamples <- builderGetObject builder castToTreeView ("treeviewSamples" :: Text)

    buttonImportInstrument <- builderGetObject builder castToButton ("buttonImportInstrument" :: Text)
    buttonExportInstrument <- builderGetObject builder castToButton ("buttonExportInstrument" :: Text)

    entryVersion <- builderGetObject builder castToEntry ("entryVersion" :: Text)
    entryName <- builderGetObject builder castToEntry ("entryName" :: Text)
    entryType <- builderGetObject builder castToEntry ("entryType" :: Text)

    popUp <- builderGetObject builder castToMenu ("menuAudioSamples" :: Text)
    menuAddSamples <- builderGetObject builder castToMenuItem ("menuitemAdd" :: Text)
    menuRemoveSample <- builderGetObject builder castToMenuItem ("menuitemRemove" :: Text)

    hitPopUp <- builderGetObject builder castToMenu ("menuHits" :: Text)
    menuAddHitSample <- builderGetObject builder castToMenuItem ("menuitemAddHitSample" :: Text)
    menuRemoveHitSample <- builderGetObject builder castToMenuItem ("menuitemRemoveHitSample" :: Text)

    -- create a tag that we use as selection, target and selection type
    sampleTypeTag <- atomNew ("_SampleType" :: Text)


    hsls <- listStoreNew []
    (rendererHPName, rendererHP) <- initTreeViewHit treeviewHit hsls

    --sals <- listStoreNewDND []
        --(Just DragSourceIface {
            --treeDragSourceRowDraggable = \_ _ -> return True,
            --treeDragSourceDragDataGet = dragSamplesAction gui sampleTypeTag,
            --treeDragSourceDragDataDelete = \_ _ -> return True
        --})
        --Nothing
    sals <- listStoreNew []

    (rendChan, rendFileChan) <- initTreeViewSamples treeviewSamples sals sampleTypeTag

    ifr <- newIORef Nothing

    let gui = InstrumentPage {
        guiInstMainBox = mainBox,
        guiMainWindow = parentWindow,
        guiIPNotebook = notebook,
        guiIPEntryBaseDir = basedir,
        guiIPEntrySamplesDir = samplesDir,
        guiInstHitView = treeviewHit,
        guiInstHitViewModel = hsls,
        guiInstSamplesView = treeviewSamples,
        guiInstSamplesViewModel = sals,
        guiInstFile = ifr,
        guiEntryVersion = entryVersion,
        guiEntryName = entryName,
        guiEntryType = entryType,
        guiRendererHP = rendererHP,
        guiAudioSamplesMenu = popUp,
        guiRendererChan = rendChan,
        guiRendererFileChan = rendFileChan,
        guiIPInstrumentPages = ioref,
        guiIPParserCombo = combo,
        guiHitSamplesMenu = hitPopUp,
        guiRendererHPName = rendererHPName
        }

    -- set the default values for this instrument page
    entrySetText entryVersion iflDefaultVersion

    -- setup the callback for the import button
    void $ on buttonImportInstrument buttonActivated (importDrumDropsInstrument gui)
    void $ on buttonExportInstrument buttonActivated (exportInstrument gui)

    void $ on entryName entryActivated (validateName gui)
    void $ on entryName focusOutEvent (liftIO (validateName gui) >> return False)
    void $ on entryType entryActivated (validateType gui)
    void $ on entryType focusOutEvent (liftIO (validateType gui) >> return False)

    void $ on menuAddSamples menuItemActivate (addSamples gui)
    void $ on menuRemoveSample menuItemActivate (removeAudioSamples gui)

    void $ on menuAddHitSample menuItemActivate (addHitPower gui)
    void $ on menuRemoveHitSample menuItemActivate (removeHitPower gui)

    void $ on treeviewSamples dragDataReceived $ dragDataReceivedSignal gui
    void $ on treeviewSamples dragDataGet $ dragDataGetSignal gui

    void $ on treeviewHit dragDataReceived $ dragDataReceivedSignalHit gui

    -- setup the local callbacks for the treeviews
    setupCallbacks gui

    return gui






dragDataReceivedSignal :: InstrumentPage -> DragContext -> Point -> InfoId -> TimeStamp -> SelectionDataM ()
dragDataReceivedSignal gui dragContext _ _ timestamp = do
    txt <- selectionDataGetURIs
    liftIO $ do
        P.putStrLn $"dragDataReceivedSignal called: " ++ show txt

        maybe (return ()) (dropAction gui) txt

        dragFinish dragContext True False timestamp
    return ()

dragDataReceivedSignalHit :: InstrumentPage -> DragContext -> Point -> InfoId -> TimeStamp -> SelectionDataM ()
dragDataReceivedSignalHit gui dragContext _ _ timestamp = do
    txt <- selectionDataGetText
    liftIO $ do
        T.putStrLn "dragDataReceivedSignalHit called!"
        maybe (return ()) (dropActionHit gui) txt

        dragFinish dragContext True False timestamp
    return ()


dragDataGetSignal :: InstrumentPage -> DragContext -> InfoId -> TimeStamp -> SelectionDataM ()
dragDataGetSignal gui _ _ _ = do
    r <- liftIO $ do
        T.putStrLn "Drag-Data-Get called"
        sel <- treeViewGetSelection (guiInstSamplesView gui)
        rows <- treeSelectionGetSelectedRows sel
        return (P.concat rows)

    selectionDataSet selectionTypeInteger r
    return ()


-- called when a drag and drop was done. The data of the drag and
-- drop is in the Text argument which is a list of filenames in
-- this case
dropAction :: InstrumentPage -> [Text] -> IO ()
dropAction gui x = do
    T.putStrLn "dropAction"
    let res = P.map checkFileNames x
    case (not.P.null.lefts) res of
        True -> displayErrorBox (guiMainWindow gui) ("Errors: " `append` (T.intercalate "\n" (lefts res)))
        False -> do
            basepath <- entryGetText (guiIPEntryBaseDir gui)
            af <- getAudioSamplesFromFiles (unpack basepath) (rights res)

            let ls = guiInstSamplesViewModel gui

            mapM_ (listStoreAppend ls) af


dropActionHit :: InstrumentPage -> Text -> IO ()
dropActionHit gui x = do
    T.putStrLn "dropActionHit called"
    return ()



checkFileNames :: Text -> Either Text FilePath
checkFileNames =
    chk
    where
        chk :: Text -> Either Text FilePath
        chk x =
            let prefix = "file://"
                x' = if prefix `T.isPrefixOf` x
                        then T.unpack (T.drop (T.length prefix) x)
                        else (T.unpack x)
                file = P.filter (/= '\r') x'
            in
            if (takeExtension file) == ".wav"
                then Right (unEscapeString file)
                else Left $ "Illegal file: " `T.append` (T.pack file)
        conv :: [Either Text FilePath] -> Either Text [FilePath]
        conv xs =
            let lef = lefts xs
            in
            if P.null lef then Right (rights xs) else Left (T.unlines lef)


getAudioSamplesFromFiles :: FilePath -> [FilePath] -> IO [AudioFile]
getAudioSamplesFromFiles basepath files = do
    res <- mapM (getAudioSampleFromFile basepath) files
    return (P.concat res)

getAudioSampleFromFile :: FilePath -> FilePath -> IO [AudioFile]
getAudioSampleFromFile basepath file = do
    info <- getFileInfo file
    let idx = [1..channels info]

    let relName x = determinePath basepath (dropFileName x) (pack (takeFileName x))

    return $ P.map (\x -> AudioFile Undefined (relName file) (fromIntegral x)) idx




importDrumDropsInstrument :: InstrumentPage -> IO ()
importDrumDropsInstrument instPage = do
    let parentWindow = guiMainWindow instPage

    dialog <- fileChooserDialogNew
                (Just $ ("Import DrumDrops Instrument from Path" :: Text))             --dialog title
                (Just parentWindow)                     --the parent window
                FileChooserActionSelectFolder                         --the kind of dialog we want
                [("gtk-cancel"                                --The buttons to display
                 ,ResponseCancel)
                 ,("gtk-open"
                 , ResponseAccept)]

    basedir <- entryGetText (guiIPEntryBaseDir instPage)
    sampleDir <- entryGetText (guiIPEntrySamplesDir instPage)

    pt <- comboBoxGetActiveText (guiIPParserCombo instPage)
    let parserType = maybe MapexParser (read . unpack) pt

    case basedir of
        "" -> do
            dial <- messageDialogNew (Just parentWindow) [DialogDestroyWithParent] MessageError ButtonsClose ("Base Directory not set!" :: Text)
            _ <- dialogRun dial
            widgetHide dial
            return ()
        _ -> do
            void $ fileChooserSetCurrentFolder dialog basedir

            widgetShow dialog
            resp <- dialogRun dialog
            case resp of
                ResponseAccept -> do
                    Just instrumentDir <- fileChooserGetFilename dialog

                    result <- importInstrument parserType basedir sampleDir instrumentDir
                    case result of
                        Right instrumentFile -> do
                            instrumentPageSetInstrumentFile instPage instrumentFile
                            return ()
                        Left err -> do
                            dial <- messageDialogNew (Just parentWindow) [DialogDestroyWithParent] MessageError ButtonsClose ("Could not import samples: " `append` err)
                            _ <- dialogRun dial
                            widgetHide dial
                            return ()
                ResponseCancel -> return ()
                ResponseDeleteEvent -> return ()
                _ -> return ()
    widgetHide dialog



instrumentPageGetMainBox :: InstrumentPage -> Box
instrumentPageGetMainBox = guiInstMainBox


initTreeViewHit :: TreeView -> ListStore HitSample -> IO (CellRendererText, CellRendererText)
initTreeViewHit tv ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Name" :: Text)
    treeViewColumnSetTitle col2 ("Hit Power" :: Text)

    renderer1 <- cellRendererTextNew
    rendererHP <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 rendererHP True

    set renderer1 [cellTextEditable := True,
                    cellTextEditableSet := True
                    ]

    set rendererHP [cellTextEditable := True,
                    cellTextEditableSet := True,
                    cellTextBackgroundColor := paleYellow,
                    cellTextBackgroundSet := True
                    ]

    cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [ cellText := hsName hs]
    cellLayoutSetAttributes col2 rendererHP ls $ \hs -> [ cellText := pack (show (hsPower hs)) ]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `T.isPrefixOf` toLower (hsName row)

    tls <- targetListNew
    targetListAdd tls targetString [TargetSameApp] dndInfoId
    treeViewEnableModelDragDest tv tls [ActionMove]


    return (renderer1, rendererHP)






initTreeViewSamples :: TreeView -> ListStore AudioFile -> TargetTag -> IO (CellRendererText, CellRendererText)
initTreeViewSamples tv ls sampleTypeTag = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew
    col3 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Channel" :: Text)
    treeViewColumnSetTitle col2 ("File" :: Text)
    treeViewColumnSetTitle col3 ("Filechannel" :: Text)

    renderer1 <- cellRendererTextNew
    renderer2 <- cellRendererTextNew
    renderer3 <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True
    cellLayoutPackStart col3 renderer3 True

    set renderer1 [cellTextEditable := True,
                    cellTextEditableSet := True,
                    cellTextBackgroundColor := paleYellow,
                    cellTextBackgroundSet := True
                    ]
    set renderer3 [cellTextEditable := True,
                    cellTextEditableSet := True,
                    cellTextBackgroundColor := paleYellow,
                    cellTextBackgroundSet := True
                    ]


    cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [ cellText := pack (show (afChannel hs))]
    cellLayoutSetAttributes col2 renderer2 ls $ \hs -> [ cellText := afPath hs ]
    cellLayoutSetAttributes col3 renderer3 ls $ \hs -> [ cellText := pack (show (afFileChannel hs)) ]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2
    _ <- treeViewAppendColumn tv col3

    -- enable multiple selection mode
    sel <- treeViewGetSelection tv
    treeSelectionSetMode sel SelectionMultiple

    -- set the search function
    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `T.isPrefixOf` toLower (pack (afPath row))


    -- enable drag and drop
    tls <- targetListNew
    targetListAddUriTargets tls 2
    targetListAddTextTargets tls 1
    --targetListAdd tls targetString [TargetOtherApp] 1
    treeViewEnableModelDragDest tv tls [ActionCopy]

    tls1 <- targetListNew
    targetListAdd tls1 sampleTypeTag [TargetSameApp] dndDragId
    treeViewEnableModelDragSource tv [Button1] tls1 [ActionCopy]


    return (renderer1, renderer3)


dndInfoId :: InfoId
dndInfoId = 1

dndDragId :: InfoId
dndDragId = 2


setCurrentNotebookLabel :: InstrumentPage -> Text -> IO ()
setCurrentNotebookLabel instPage name = do
    let notebook = guiIPNotebook instPage
    currentPage <- notebookGetCurrentPage notebook
    page <- notebookGetNthPage notebook currentPage
    case page of
        Just p -> notebookSetTabLabelText notebook p name
        Nothing -> return ()






instrumentPageSetInstrumentFile :: InstrumentPage -> InstrumentFile -> IO ()
instrumentPageSetInstrumentFile instPage instrumentFile = do
    let ref = guiInstFile instPage
    writeIORef ref (Just instrumentFile)

    setNotebookCurrentPageLabel instPage (ifName instrumentFile)

    entrySetText (guiEntryVersion instPage) (ifVersion instrumentFile)
    entrySetText (guiEntryName instPage) (ifName instrumentFile)
    entrySetText (guiEntryType instPage) ((pack.show.ifType) instrumentFile)

    let model = guiInstHitViewModel instPage

    setListStoreTo model (ifSamples instrumentFile)
    listStoreClear (guiInstSamplesViewModel instPage)

    return ()



setupCallbacks :: InstrumentPage -> IO ()
setupCallbacks instPage = do
    let hitview = guiInstHitView instPage
        hitviewModel = guiInstHitViewModel instPage

    -- on doubleclick on a hit sample show the assigned audio samples
    void $ on hitview rowActivated $ \(i:_) _ -> do
        !row <- listStoreGetValue hitviewModel i
        setListStoreTo (guiInstSamplesViewModel instPage) (hsSamples row)
        return ()

    -- right click on the samples view shows the popup to add/remove audio samples)
    void $ on (guiInstSamplesView instPage) buttonPressEvent $ do
        bt <- eventButton
        case bt of
            RightButton -> do
                liftIO $ do
                    res <- treeViewIsSelected (guiInstHitView instPage)
                    when res $ do
                        menuPopup (guiAudioSamplesMenu instPage) Nothing
                return True
            _ -> return False

    -- right click on the hit sample view shows the popup for add/remove
    void $ on (guiInstHitView instPage) buttonPressEvent $ do
        bt <- eventButton
        case bt of
            RightButton -> do
                liftIO $ menuPopup (guiHitSamplesMenu instPage) Nothing
                return True
            _ -> return False

    -- edit call back for editing the hit power in the hit sample view
    void $ on (guiRendererHP instPage) edited $ \[i] str -> do
        val <- listStoreGetValue (guiInstHitViewModel instPage) i
        let res = checkFloat str
        case res of
            Left err -> displayErrorBox (guiMainWindow instPage) err
            Right x -> do
                -- set the GTK list store to the new value
                listStoreSetValue (guiInstHitViewModel instPage) i (val {hsPower = x})

    -- edit callback for editing the name of the hit sample
    void $ on (guiRendererHPName instPage) edited $ \[i] str -> do
        val <- listStoreGetValue (guiInstHitViewModel instPage) i
        let res = T.filter (not . isSpace) str
        -- set the GTK list store to the new value
        listStoreSetValue (guiInstHitViewModel instPage) i (val {hsName = res})


    void $ on (guiRendererChan instPage) edited $ \[i] str -> do
        val <- listStoreGetValue (guiInstSamplesViewModel instPage) i
        let res = validateMic str
        case res of
            Left err -> displayErrorBox (guiMainWindow instPage) err
            Right x -> do
                -- set the GTK list store to the new value
                let val' = val {afChannel = x}
                listStoreSetValue (guiInstSamplesViewModel instPage) i val'
                -- we also need to set the new value in the HitSample itself

                sel <- treeViewGetSelection (guiInstHitView instPage)
                path <- treeSelectionGetSelectedRows sel
                let idx = P.head (P.head path)
                hsVal <- listStoreGetValue (guiInstHitViewModel instPage) idx
                let samples = hsSamples hsVal
                    samples' = fmap upd samples
                    upd s = if s == val then val' else s
                    hsVal' = hsVal {hsSamples = samples'}
                listStoreSetValue (guiInstHitViewModel instPage) idx hsVal'

    void $ on (guiRendererFileChan instPage) edited $ \[i] str -> do
        val <- listStoreGetValue (guiInstSamplesViewModel instPage) i
        let res = validate str
        case res of
            Left err -> displayErrorBox (guiMainWindow instPage) err
            Right x -> do
                -- set the GTK list store to the new value
                let val' = val {afFileChannel = x}
                listStoreSetValue (guiInstSamplesViewModel instPage) i val'
                -- we also need to set the new value in the HitSample itself

                sel <- treeViewGetSelection (guiInstHitView instPage)
                path <- treeSelectionGetSelectedRows sel
                let idx = P.head (P.head path)
                hsVal <- listStoreGetValue (guiInstHitViewModel instPage) idx
                let samples = hsSamples hsVal
                    samples' = fmap upd samples
                    upd s = if s == val then val' else s
                    hsVal' = hsVal {hsSamples = samples'}
                listStoreSetValue (guiInstHitViewModel instPage) idx hsVal'

    return ()


treeViewIsSelected :: TreeView -> IO Bool
treeViewIsSelected tv = do
    sel <- treeViewGetSelection tv
    rows <- treeSelectionGetSelectedRows sel
    return $ (not . P.null) rows



addSamples :: InstrumentPage -> IO ()
addSamples gui = do
    names <- loadSamples gui
    when (not (P.null names)) $ do
        basepath <- unpack <$> entryGetText (guiIPEntryBaseDir gui)
        af <- getAudioSamplesFromFiles basepath names
        mapM_ (listStoreAppend (guiInstSamplesViewModel gui)) af

        -- also update the hit sample
        sel <- treeViewGetSelection (guiInstHitView gui)
        path <- treeSelectionGetSelectedRows sel
        let idx = P.head (P.head path)
        hsVal <- listStoreGetValue (guiInstHitViewModel gui) idx
        let samples = hsSamples hsVal
            samples' = samples ++ af
            hsVal' = hsVal {hsSamples = samples'}
        listStoreSetValue (guiInstHitViewModel gui) idx hsVal'


loadSamples :: InstrumentPage -> IO [FilePath]
loadSamples instPage = do
    let parentWindow = guiMainWindow instPage

    dialog <- fileChooserDialogNew
                (Just $ ("Load Samples" :: Text))             --dialog title
                (Just parentWindow)                     --the parent window
                FileChooserActionOpen                         --the kind of dialog we want
                [("gtk-cancel"                                --The buttons to display
                 ,ResponseCancel)
                 ,("gtk-open"
                 , ResponseAccept)]

    fileChooserSetSelectMultiple dialog True

    --basedir <- entryGetText (guiIPEntryBaseDir instPage) :: IO Text
    sampleDir <- entryGetText (guiIPEntrySamplesDir instPage)

    res <- case sampleDir of
        "" -> do
            dial <- messageDialogNew (Just parentWindow) [DialogDestroyWithParent] MessageError ButtonsClose ("Sample Directory not set!" :: Text)
            _ <- dialogRun dial
            widgetHide dial
            return []
        _ -> do
            void $ fileChooserSetCurrentFolder dialog sampleDir

            widgetShow dialog
            resp <- dialogRun dialog
            case resp of
                ResponseAccept -> fileChooserGetFilenames dialog
                ResponseCancel -> return []
                ResponseDeleteEvent -> return []
                _ -> return []
    widgetHide dialog
    return res


removeAudioSamples :: InstrumentPage -> IO ()
removeAudioSamples gui = do
    -- get selected samples and remove them
    sel <- treeViewGetSelection (guiInstSamplesView gui)
    rows' <- treeSelectionGetSelectedRows sel

    -- remove from ListStore
    let ls = guiInstSamplesViewModel gui
        rows = P.map P.head rows'

    -- get an intermediate list of values to be removed
    samples <- mapM (listStoreGetValue ls) rows

    -- remove from Hit Sample
    sel1 <- treeViewGetSelection (guiInstHitView gui)
    selHit <- P.head . P.head <$> treeSelectionGetSelectedRows sel1
    hs <- listStoreGetValue (guiInstHitViewModel gui) selHit
    -- set new Hit Sample
    listStoreSetValue (guiInstHitViewModel gui) selHit (removeSamples hs samples)

    -- activate the row so that the audio sample view is refreshed
    Just col <- treeViewGetColumn (guiInstHitView gui) 0
    treeViewRowActivated (guiInstHitView gui) [selHit] col


addHitPower :: InstrumentPage -> IO ()
addHitPower gui = do
    instName <- entryGetText (guiEntryName gui)
    n <- listStoreGetSize (guiInstHitViewModel gui)
    let defSample = HitSample smplName 1.0 []
        smplName = instName `append` "-" `append` pack (show (n + 1))
    idx <- listStoreAppend (guiInstHitViewModel gui) defSample
    treeViewSetCursor (guiInstHitView gui) [idx] Nothing


removeHitPower :: InstrumentPage -> IO ()
removeHitPower gui = do
    sel1 <- treeViewGetSelection (guiInstHitView gui)
    selHit <- P.head . P.head <$> treeSelectionGetSelectedRows sel1
    listStoreRemove (guiInstHitViewModel gui) selHit

    -- activate the row so that the audio sample view is refreshed
    selHitAct <- P.head . P.head <$> treeSelectionGetSelectedRows sel1
    Just col <- treeViewGetColumn (guiInstHitView gui) 0
    treeViewRowActivated (guiInstHitView gui) [selHitAct] col



getInstrumentFromGUI :: InstrumentPage -> IO (Either Text InstrumentFile)
getInstrumentFromGUI instPage = do
    name <- entryGetText (guiEntryName instPage)
    t <- entryGetText (guiEntryType instPage)
    samples <- listStoreToList (guiInstHitViewModel instPage)

    T.putStrLn $ name `append` " Type: " `append` t

    let version = dgDefaultVersion
        t' = validate t

    case t' of
        Left err  -> return $ Left err
        Right typ -> do
            let res = InstrumentFile version name typ samples
            return (Right res)


storeInstrument :: InstrumentPage -> InstrumentFile -> IO ()
storeInstrument instPage instFile = do
    writeIORef (guiInstFile instPage) (Just instFile)



exportInstrument :: InstrumentPage -> IO ()
exportInstrument instPage = do
    res <- instrumentPageWriteInstrumentFile instPage
    case res of
        Left err -> displayErrorBox (guiMainWindow instPage) ("Error during export: " `append` err)
        Right filename -> displayInfoBox (guiMainWindow instPage)
            ("Successfully exported instrument (" `append` pack filename `append` ")")



instrumentPageWriteInstrumentFile :: InstrumentPage -> IO (Either Text FilePath)
instrumentPageWriteInstrumentFile instPage = do
    i <- getInstrumentFromGUI instPage
    case i of
        Left err -> return $ Left err
        Right instrumentFile -> do
            storeInstrument instPage instrumentFile

            basepath <- entryGetText (guiIPEntryBaseDir instPage)

            let
                dgInstrumentsPath = getInstrumentDir basepath
                --content = convertToInstrumentXML instrumentFile
                filename = dgInstrumentsPath </> T.unpack (ifName instrumentFile) <.> "xml"

            writeInstrumentXML instrumentFile filename
            --B.writeFile filename content

            return $ Right filename


validateName :: InstrumentPage -> IO ()
validateName instPage = do
    nm <- entryGetText (guiEntryName instPage)
    let nm' = T.filter (not . isSpace) nm
    entrySetText (guiEntryName instPage) nm'
    setCurrentNotebookLabel instPage nm'



validate :: Read a => Text -> (Either Text a)
validate input = do
    let maybeRead = (fmap fst . listToMaybe . reads) (unpack input)
    case maybeRead of
        Just x -> Right x
        Nothing -> Left ("Illegal input: '" `append` input `append` "'")


validateType :: InstrumentPage -> IO ()
validateType instPage = do
    t <- entryGetText (guiEntryType instPage)
    let r = validate t
    case r of
        Left err -> displayErrorBox (guiMainWindow instPage) (err `append` allowedTypes)
        Right x -> do
            iF <- readIORef (guiInstFile instPage)
            case iF of
                Nothing -> return ()
                Just instF -> do
                    let !iF' = instF {ifType = x}
                    writeIORef (guiInstFile instPage) (Just iF')
    where
        allowedTypes = "\nAllowedTypes:\n\nKick, Snare, HiHat, Tom TomType, Cymbal, Ride, Shaker, Tambourine\n where TomType is either: RackTom <n> or Floor <n>"



instrumentPageInsert :: InstrumentPage -> IO ()
instrumentPageInsert instPage = do
    let ioref = guiIPInstrumentPages instPage
    modifyIORef' ioref (\v -> v V.++ V.singleton instPage)
    return ()


setNotebookCurrentPageLabel :: InstrumentPage -> Text -> IO ()
setNotebookCurrentPageLabel instPage name = do
    let notebook = guiIPNotebook instPage
    currentPage <- notebookGetCurrentPage notebook
    page <- notebookGetNthPage notebook currentPage
    case page of
        Just p -> notebookSetTabLabelText notebook p name
        Nothing -> return ()


instrumentPageReset :: InstrumentPage -> IO ()
instrumentPageReset gui = do
    listStoreClear (guiInstSamplesViewModel gui)
    listStoreClear (guiInstHitViewModel gui)

    writeIORef (guiInstFile gui) Nothing

    n <- notebookGetNPages (guiIPNotebook gui)
    forM_ [0..(n-1)] $ \_ ->
        notebookRemovePage (guiIPNotebook gui) (-1)

    return ()

instrumentPageSetInstrumentName :: InstrumentPage -> Text -> IO ()
instrumentPageSetInstrumentName gui name = do
    let nm' = T.filter (not . isSpace) name
    entrySetText (guiEntryName gui) nm'
    setCurrentNotebookLabel gui nm'
