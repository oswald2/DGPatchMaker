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
    ,instrumentPageGetInstrumentName
    ,instrumentPageGetInstrumentFile
    ,instrumentPageLoadFile
    )
where

import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket, catch, SomeException(..))

import Prelude as P

import Graphics.UI.Gtk

import Gtk.Colors
import Gtk.Utils
import Gtk.FileHandlingDialog

import Data.Text as T
import Data.Text.IO as T

import Data.Types
import Data.IORef
import Data.Checkers
import Data.Drumgizmo
import Data.Export
import Data.Import
import Data.Maybe
import Data.Char (isSpace)
import Data.Either
import Data.List as L (elemIndex)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Data.DrumDrops.Utils

import System.FilePath

import Gtk.InstrumentPageBuilder
import Gtk.ErrorDialog
import Gtk.NrHitsDialog

import Network.URI (unEscapeString)

import Sound.File.Sndfile as SF (getFileInfo, Info(..), IOMode(..), openFile, hGetBuffer, hClose)
import Sound.File.Sndfile.Buffer.Vector as BV

import Text.Printf


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
    guiRendererChan :: CellRendererCombo,
    guiRendererFileChan :: CellRendererText,
    guiInstSamplesView :: TreeView,
    guiInstSamplesViewModel :: ListStore AudioFile,
    guiInstFile :: IORef (Maybe InstrumentFile),
    guiEntryVersion :: Entry,
    guiEntryName :: Entry,
    guiEntryType :: Entry,
    guiAudioSamplesMenu :: Menu,
    guiIPParserCombo :: ComboBox,
    guiHitSamplesMenu :: Menu,
    guiSpinAttack :: SpinButton,
    guiSpinSpread :: SpinButton,
    guiInstFhDialog :: FileHandlingDialog,
    guiInstErrDialog :: ErrorDialog,
    guiInstNrHits :: NrHitsDialog,
    guiComboChannel :: ComboBox
    }




instrumentPageNew :: Window -> Notebook -> Entry -> Entry -> ComboBox -> IORef (V.Vector InstrumentPage) -> FileHandlingDialog -> ErrorDialog -> IO InstrumentPage
instrumentPageNew parentWindow notebook basedir samplesDir combo ioref fhDialog errDiag = do
    -- Create the builder, and load the UI file
    builder <- builderNew

    --builderAddFromFile builder "InstrumentPage.glade"
    builderAddFromString builder builderFileAsString

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
    menuSelectFC <- builderGetObject builder castToMenuItem ("menuitemSelectFC" :: Text)
    menuAddNewHS <- builderGetObject builder castToMenuItem ("menuitemToNewHS" :: Text)

    hitPopUp <- builderGetObject builder castToMenu ("menuHits" :: Text)
    menuAddHitSample <- builderGetObject builder castToMenuItem ("menuitemAddHitSample" :: Text)
    menuRemoveHitSample <- builderGetObject builder castToMenuItem ("menuitemRemoveHitSample" :: Text)
    menuAddNrHits <- builderGetObject builder castToMenuItem ("menuitemAddMultiple" :: Text)

    spinAttack <- builderGetObject builder castToSpinButton ("spinbuttonAttack" :: Text)
    spinSpread <- builderGetObject builder castToSpinButton ("spinbuttonSpread" :: Text)
    calcHitB <- builderGetObject builder castToButton ("buttonCalcHits" :: Text)
    loadInst <- builderGetObject builder castToButton ("buttonLoadInstrument" :: Text)

    comboChan <- builderGetObject builder castToComboBox ("comboboxChannel" :: Text)
    initChannelCombo comboChan

    nrHitsDiag <- initNrHitsDialog builder

    hsls <- listStoreNew []
    (rendererHPName, rendererHP) <- initTreeViewHit treeviewHit hsls

    sals <- listStoreNew []
    (rendChan, rendFileChan) <- initTreeViewSamples treeviewSamples sals

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
        guiRendererHPName = rendererHPName,
        guiSpinAttack = spinAttack,
        guiSpinSpread = spinSpread,
        guiInstFhDialog = fhDialog,
        guiInstErrDialog = errDiag,
        guiInstNrHits = nrHitsDiag,
        guiComboChannel = comboChan
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
    void $ on menuSelectFC menuItemActivate (selectAllFC gui)
    void $ on menuAddNewHS menuItemActivate (toNewHitSample gui)

    void $ on menuAddHitSample menuItemActivate (addHitPower gui)
    void $ on menuRemoveHitSample menuItemActivate (removeHitPower gui)
    void $ on menuAddNrHits menuItemActivate (addMultipleHits gui)

    void $ on treeviewSamples dragDataReceived $ dragDataReceivedSignal gui
    void $ on treeviewSamples dragDataGet $ dragDataGetSignal gui

    void $ on treeviewHit dragDataReceived $ dragDataReceivedSignalHit gui

    void $ on calcHitB buttonActivated (calcPower gui)
    void $ on loadInst buttonActivated (loadInstrument gui)

    void $ on comboChan changed (changeChannel gui)

    -- setup the local callbacks for the treeviews
    setupCallbacks gui

    return gui






dragDataReceivedSignal :: InstrumentPage -> DragContext -> Point -> InfoId -> TimeStamp -> SelectionDataM ()
dragDataReceivedSignal gui dragContext _ _ timestamp = do
    txt <- selectionDataGetURIs
    liftIO $ do
        b <- treeViewIsSelected (guiInstHitView gui)
        when b $ maybe (return ()) (dropAction gui) txt
        dragFinish dragContext True False timestamp
    return ()

dragDataReceivedSignalHit :: InstrumentPage -> DragContext -> Point -> InfoId -> TimeStamp -> SelectionDataM ()
dragDataReceivedSignalHit gui dragContext point _ timestamp = do
    txt <- selectionDataGetText
    liftIO $ do
        maybe (return ()) (dropActionHit gui point) txt

        dragFinish dragContext True False timestamp
    return ()


dragDataGetSignal :: InstrumentPage -> DragContext -> InfoId -> TimeStamp -> SelectionDataM ()
dragDataGetSignal gui _ _ _ = do
    r <- liftIO $ do
        sel <- treeViewGetSelection (guiInstSamplesView gui)
        rows <- treeSelectionGetSelectedRows sel
        let ls = guiInstSamplesViewModel gui
        mapM (listStoreGetValue ls) (P.concat rows)

    void $ selectionDataSetText (show r)
    return ()


-- called when a drag and drop was done. The data of the drag and
-- drop is in the Text argument which is a list of filenames in
-- this case
dropAction :: InstrumentPage -> [Text] -> IO ()
dropAction gui x = do
    let res = P.map checkFileNames x
    case (not.P.null.lefts) res of
        True -> displayErrorBox (guiMainWindow gui) ("Errors: " `append` (T.intercalate "\n" (lefts res)))
        False -> do
            basepath <- entryGetText (guiIPEntryBaseDir gui)
            af <- getAudioSamplesFromFiles (unpack basepath) (rights res)

            let ls = guiInstSamplesViewModel gui

            mapM_ (listStoreAppend ls) af
            updateHitSample gui hsAddSamples af


dropActionHit :: InstrumentPage -> Point -> Text -> IO ()
dropActionHit gui point txt = do
    -- get the drag and drop data from text into data
    let afs :: [AudioFile]
        afs = read (unpack txt)

    -- check, if we are with the mouse pointer over a hit sample
    let tv = guiInstHitView gui
    pos <- treeViewConvertWidgetToTreeCoords (guiInstHitView gui) point
    res <- treeViewGetPathAtPos tv pos
    case res of
        Nothing -> return ()
        Just ((idx:_), _, _) -> do
            -- get the selected hit sample and remove the audio samples
            -- then get the drop destination hit sample and add the samples
            sel <- treeViewGetSelection (guiInstHitView gui)
            ((srcx:_):_) <- treeSelectionGetSelectedRows sel
            let ls = guiInstHitViewModel gui
            src <- listStoreGetValue ls srcx
            listStoreSetValue ls srcx (hsRemoveSamples src afs)

            dest <- listStoreGetValue ls idx
            listStoreSetValue ls idx (hsAddSamples dest afs)

            -- activate the row so that the audio sample view is refreshed
            activateRow (guiInstHitView gui) srcx

        _ -> return ()


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


getAudioSamplesFromFiles :: FilePath -> [FilePath] -> IO [AudioFile]
getAudioSamplesFromFiles basepath files = do
    res <- mapM (getAudioSampleFromFile basepath) files
    return (P.concat res)

getAudioSampleFromFile :: FilePath -> FilePath -> IO [AudioFile]
getAudioSampleFromFile basepath file = do
    info <- getFileInfo file
    let idx = [1..channels info]

    let relName x = determinePath basepath (dropFileName x) (pack (takeFileName x))

    return $ P.map (\x -> AudioFile (pack (showMic Undefined)) (relName file) (fromIntegral x) Nothing) idx




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
    targetListAddTextTargets tls dndDragId
    treeViewEnableModelDragDest tv tls [ActionCopy]

    return (renderer1, rendererHP)






initTreeViewSamples :: TreeView -> ListStore AudioFile -> IO (CellRendererCombo, CellRendererText)
initTreeViewSamples tv ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew
    col3 <- treeViewColumnNew
    col4 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Channel" :: Text)
    treeViewColumnSetTitle col2 ("File" :: Text)
    treeViewColumnSetTitle col3 ("Filechannel" :: Text)
    treeViewColumnSetTitle col4 ("Power" :: Text)

    treeViewColumnSetResizable col1 True
    treeViewColumnSetSizing col1 TreeViewColumnAutosize

    renderer1 <- cellRendererComboNew
    renderer2 <- cellRendererTextNew
    renderer3 <- cellRendererTextNew
    renderer4 <- cellRendererTextNew

    lsChans <- listStoreNew chanList

    let colId :: ColumnId Text Text
        colId = makeColumnIdString 0

    treeModelSetColumn lsChans colId id

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True
    cellLayoutPackStart col3 renderer3 True
    cellLayoutPackStart col4 renderer4 True

    set renderer1 [cellTextEditable := True,
                    cellTextEditableSet := True,
                    cellTextBackgroundColor := paleYellow,
                    cellTextBackgroundSet := True,

                    cellComboHasEntry := True,
                    cellComboTextModel := (lsChans, colId)
                    ]
    set renderer3 [cellTextEditable := True,
                    cellTextEditableSet := True,
                    cellTextBackgroundColor := paleYellow,
                    cellTextBackgroundSet := True
                    ]


    cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [ cellText := afChannel hs, cellComboTextModel := (lsChans, colId)]
    cellLayoutSetAttributes col2 renderer2 ls $ \hs -> [ cellText := afPath hs ]
    cellLayoutSetAttributes col3 renderer3 ls $ \hs -> [ cellText := pack (show (afFileChannel hs)) ]
    cellLayoutSetAttributes col4 renderer4 ls $ \hs -> [ cellText := showPower hs ]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2
    _ <- treeViewAppendColumn tv col3
    _ <- treeViewAppendColumn tv col4

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
    targetListAddTextTargets tls1 dndDragId
    treeViewEnableModelDragSource tv [Button1] tls1 [ActionCopy]


    return (renderer1, renderer3)

showPower :: AudioFile -> Text
showPower af =
    case afPower af of
        Nothing -> "--"
        Just x -> pack (printf "%10.5f" x)


dndDragId :: InfoId
dndDragId = 2



chanList :: [Text]
chanList = P.map (pack.showMic)
    [KickC,
     KickL,
     KickR,
     KickS,
     SnareTop,
     SnareBottom,
     SnareL,
     SnareR,
     HiHatC,
     HiHatL,
     HiHatR,
     TomC 1,
     TomL 1,
     TomR 1,
     TomC 2,
     TomL 2,
     TomR 2,
     TomC 3,
     TomL 3,
     TomR 3,
     FloorTomC 1,
     FloorTomL 1,
     FloorTomR 1,
     FloorTomC 2,
     FloorTomL 2,
     FloorTomR 2,
     RideC,
     RideL,
     RideR,
     OHL,
     OHR,
     RoomL,
     RoomR,
     Room1Mono,
     Room2Mono,
     FullMixL,
     FullMixR,
     ShakerC,
     TambourineC,
     Undefined]

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
    entrySetText (guiEntryType instPage) (maybe "" (pack . show) (ifType instrumentFile))

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

    -- on doubleclick on a audio sample set the channel combo box
    void $ on (guiInstSamplesView instPage) rowActivated $ \(i:_) _ -> do
        !row <- listStoreGetValue (guiInstSamplesViewModel instPage) i
        let chan = afChannel row
            idx = elemIndex chan chanList
        case idx of
            Nothing -> return ()
            Just x -> comboBoxSetActive (guiComboChannel instPage) x
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

    -- callback for editing the channel
    void $ on (guiRendererChan instPage) edited $ \[_] stri -> do
        -- do the editing for all selected audio files
        sel1 <- treeViewGetSelection (guiInstHitView instPage)
        hitSampleP <- treeSelectionGetSelectedRows sel1

        sel <- treeViewGetSelection (guiInstSamplesView instPage)
        paths <- treeSelectionGetSelectedRows sel

        let
            setSingleChannel :: Text -> [TreePath] -> Int -> IO ()
            setSingleChannel str hitSamplePath i = do
                val <- listStoreGetValue (guiInstSamplesViewModel instPage) i
                let res = validateMic str
                case res of
                    Left err -> displayErrorBox (guiMainWindow instPage) err
                    Right _ -> do
                        -- set the GTK list store to the new value
                        let val' = val {afChannel = str}
                        listStoreSetValue (guiInstSamplesViewModel instPage) i val'
                        -- we also need to set the new value in the HitSample itself

                        case hitSamplePath of
                            ((idx:_) : _) -> do
                                hsVal <- listStoreGetValue (guiInstHitViewModel instPage) idx
                                let samples = hsSamples hsVal
                                    samples' = fmap upd samples
                                    upd s = if s == val then val' else s
                                    hsVal' = hsVal {hsSamples = samples'}
                                listStoreSetValue (guiInstHitViewModel instPage) idx hsVal'
                            _ -> return ()

        mapM_ (setSingleChannel stri hitSampleP) ((P.map P.head . P.filter (not.P.null)) paths)


    -- callback for editing the file channel
    void $ on (guiRendererFileChan instPage) edited $ \[i] str -> do
        val <- listStoreGetValue (guiInstSamplesViewModel instPage) i
        let res = validate "FileChannel" str
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

    -- callback for setting up the combo box renderer used for setting the channel
    -- Unfortunately this is necessary for GTK3 as it otherwise doesn't work
    void $ on (guiRendererChan instPage) editingStarted $ \widget treepath -> do
        case treepath of
            [_] -> do
                comboListStore <- comboBoxSetModelText (castToComboBox widget)
                mapM_ (listStoreAppend comboListStore) chanList
            _ -> return ()

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

        updateHitSample gui hsAddSamples af


updateHitSample :: InstrumentPage -> (HitSample -> [AudioFile] -> HitSample) -> [AudioFile] -> IO ()
updateHitSample gui f af = do
    -- also update the hit sample
    sel <- treeViewGetSelection (guiInstHitView gui)
    path <- treeSelectionGetSelectedRows sel
    case path of
        ((idx:_):_) -> do
            hsVal <- listStoreGetValue (guiInstHitViewModel gui) idx
            listStoreSetValue (guiInstHitViewModel gui) idx (f hsVal af)
        _ -> return ()


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
    listStoreSetValue (guiInstHitViewModel gui) selHit (hsRemoveSamples hs samples)

    -- activate the row so that the audio sample view is refreshed
    activateRow (guiInstHitView gui) selHit


addHitPower :: InstrumentPage -> IO ()
addHitPower gui = do
    addMultipleHits' gui 1


addMultipleHits' :: InstrumentPage -> Int -> IO ()
addMultipleHits' gui howMany = do
    instName <- entryGetText (guiEntryName gui)
    j <- listStoreGetSize (guiInstHitViewModel gui)
    let defSample x = HitSample (smplName x) (fromIntegral x) []
        smplName x = instName `append` "-" `append` pack (show x)
        lst = P.map defSample [(j + 1) .. (j + howMany)]
    idxs <- mapM (listStoreAppend (guiInstHitViewModel gui)) lst
    let idx = P.last idxs
    treeViewSetCursor (guiInstHitView gui) [idx] Nothing
    activateRow (guiInstHitView gui) idx


addMultipleHits :: InstrumentPage -> IO ()
addMultipleHits gui = do
    res <- dialogGetNrHits (guiInstNrHits gui)
    case res of
        Nothing -> return ()
        Just n -> addMultipleHits' gui n


removeHitPower :: InstrumentPage -> IO ()
removeHitPower gui = do
    sel1 <- treeViewGetSelection (guiInstHitView gui)
    selHit <- treeSelectionGetSelectedRows sel1
    case selHit of
        ((x:_):_) -> listStoreRemove (guiInstHitViewModel gui) x
        _ -> return ()

    -- activate the row so that the audio sample view is refreshed
    selHitAct <- treeSelectionGetSelectedRows sel1
    case selHitAct of
        ((x:_):_) -> activateRow (guiInstHitView gui) x
        _ -> return ()



getInstrumentFromGUI :: InstrumentPage -> IO (Either Text InstrumentFile)
getInstrumentFromGUI instPage = do
    name <- entryGetText (guiEntryName instPage)
    t <- entryGetText (guiEntryType instPage)
    samples <- listStoreToList (guiInstHitViewModel instPage)

    T.putStrLn $ name `append` " Type: " `append` t

    let version = dgDefaultVersion
        t' = validate "Type" t

    case t' of
        Left _  -> do
            let res = InstrumentFile version name Nothing samples
            return (Right res)
        Right typ -> do
            let res = InstrumentFile version name (Just typ) samples
            return (Right res)


storeInstrument :: InstrumentPage -> InstrumentFile -> IO ()
storeInstrument instPage instFile = do
    writeIORef (guiInstFile instPage) (Just instFile)



exportInstrument :: InstrumentPage -> IO ()
exportInstrument instPage = do
    -- this function is only called from the InstrumentPage GUI and therefore we have out own
    -- file overwrite handling dialog, so we reset it here
    -- If the whole drumkit is exported, the withFileHandlingDialog will be called
    -- from the DrumkitPage and therefore have the correct scope for the
    -- whole export
    res <- withFileHandlingDialog (guiInstFhDialog instPage) $ instrumentPageWriteInstrumentFile instPage
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
                filename = dgInstrumentsPath </> T.unpack (ifName instrumentFile) <.> "xml"

            dirs <- createDrumgizmoDirectories basepath
            case dirs of
                Left err -> return $ Left err
                Right _ -> do
                    askUserForOverwriteIfNecessary (guiInstFhDialog instPage) filename $ writeInstrumentXML instrumentFile filename
                    return $ Right filename


validateName :: InstrumentPage -> IO ()
validateName instPage = do
    nm <- entryGetText (guiEntryName instPage)
    let nm' = T.filter (not . isSpace) nm
    entrySetText (guiEntryName instPage) nm'
    setCurrentNotebookLabel instPage nm'



validate :: Read a => Text -> Text -> (Either Text a)
validate msg input = do
    let maybeRead = (fmap fst . listToMaybe . reads) (unpack input)
    case maybeRead of
        Just x -> Right x
        Nothing -> Left ("Illegal input for " `append` msg `append` " '" `append` input `append` "'")


validateType :: InstrumentPage -> IO ()
validateType instPage = do
    t <- entryGetText (guiEntryType instPage)
    let r = validate "Type" t
    case r of
        Left err -> displayErrorBox (guiMainWindow instPage) (err `append` allowedTypes)
        Right x -> do
            iF <- readIORef (guiInstFile instPage)
            case iF of
                Nothing -> return ()
                Just instF -> do
                    let !iF' = instF {ifType = Just x}
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

instrumentPageGetInstrumentName :: InstrumentPage -> IO Text
instrumentPageGetInstrumentName gui = entryGetText (guiEntryName gui)

instrumentPageGetInstrumentFile :: InstrumentPage -> IO (Either Text InstrumentFile)
instrumentPageGetInstrumentFile gui = do
    i <- getInstrumentFromGUI gui
    case i of
        Left err -> do
            name <- instrumentPageGetInstrumentName gui
            return $ Left (name `append` ": " `append` err)
        Right instrumentFile -> do
            storeInstrument gui instrumentFile
            return $ Right instrumentFile



calcPower :: InstrumentPage -> IO ()
calcPower gui = do
    -- get the values from the gui
    attack <- round <$> spinButtonGetValue (guiSpinAttack gui)
    spread <- spinButtonGetValue (guiSpinSpread gui)
    basepath <- unpack <$> entryGetText (guiIPEntryBaseDir gui)

    dirs <- createDrumgizmoDirectories basepath
    case dirs of
        Left err -> displayErrorBox (guiMainWindow gui) ("Could not create Drumgizmo directory. This is necessary for getting the paths to the WAV files correctly for the file processing: "
                        `append` err)
        Right _ -> do
            afs <- listStoreToList (guiInstSamplesViewModel gui)

            when (not (P.null afs)) $ do
                -- do the calculation
                res <- mapM (calcHit basepath attack spread) afs

                if not (P.null (lefts res))
                    then do
                        displayMultiErrors (guiInstErrDialog gui) "Errors during processing files:" (lefts res)
                    else do
                        let files = rights res
                        setListStoreTo (guiInstSamplesViewModel gui) files

                        -- update the hit view
                        updateHitSample gui hsReplaceSamples files

    where
        calcHit :: FilePath -> Int -> Double -> AudioFile -> IO (Either Text AudioFile)
        calcHit basepath attack spread x = do
            catch (calcHit' basepath attack spread x >>= return . Right)
                  (\e -> do
                    let path = getInstrumentDir basepath </> afPath x
                    return $ Left ("Error reading file: " `append` pack path `append` ": " `append` (pack (show (e :: SomeException)))))
        calcHit' :: FilePath -> Int -> Double -> AudioFile -> IO AudioFile
        calcHit' basepath attack spread x = do
            let path = getInstrumentDir basepath </> afPath x
            T.putStrLn $ "Calc Hit Power path: " `append` pack path
            info <- getFileInfo path
            bracket (openFile path ReadMode info)
                    (hClose)
                    (\h -> do
                        b <- hGetBuffer h attack
                        case b of
                            Nothing -> return x
                            Just buf -> do
                                let v :: VS.Vector Double
                                    v = BV.fromBuffer buf
                                    !hp = performCalc attack spread v
                                return (x {afPower = Just hp})
                    )
        performCalc :: Int -> Double -> VS.Vector Double -> Double
        performCalc attack spread v = (f v) ** s
            where
                s = spread / 1000
                f = VS.foldl' (\a b -> a + b * b) 0.0 . VS.take attack


loadInstrument :: InstrumentPage -> IO ()
loadInstrument gui = do
    f <- getXMLFile gui
    case f of
        Nothing -> return ()
        Just fname -> instrumentPageLoadFile gui fname



instrumentPageLoadFile :: InstrumentPage -> FilePath -> IO ()
instrumentPageLoadFile gui fname = do
    ifr <- importInstrumentFile fname
    case ifr of
        Left err -> displayErrorBox (guiMainWindow gui) $ "Could not load the file:" `append` (pack fname) `append` ":\n" `append` err
        Right iF -> do
            instrumentPageSetInstrumentFile gui iF




getXMLFile :: InstrumentPage -> IO (Maybe FilePath)
getXMLFile gui = do
    let parentWindow = guiMainWindow gui

    dialog <- fileChooserDialogNew
                (Just $ ("Load Instrument File" :: Text))             --dialog title
                (Just parentWindow)                     --the parent window
                FileChooserActionOpen                         --the kind of dialog we want
                [("gtk-cancel"                                --The buttons to display
                 ,ResponseCancel)
                 ,("gtk-open"
                 , ResponseAccept)]

    basedir <- entryGetText (guiIPEntryBaseDir gui) :: IO Text
    let dgdir = getDrumgizmoDir (unpack basedir)

    void $ fileChooserSetCurrentFolder dialog dgdir

    xmlFilter <- fileFilterNew
    fileFilterSetName xmlFilter ("XML" :: Text)
    fileFilterAddPattern xmlFilter ("*.xml" :: Text)
    fileChooserSetFilter dialog xmlFilter

    widgetShow dialog
    resp <- dialogRun dialog
    res <- case resp of
        ResponseAccept -> fileChooserGetFilename dialog
        _ -> return Nothing
    widgetHide dialog
    return res



selectAllFC :: InstrumentPage -> IO ()
selectAllFC gui = do
    sel <- treeViewGetSelection (guiInstSamplesView gui)
    s <- treeSelectionGetSelectedRows sel
    case s of
        ((i:_):_) -> do
            val <- listStoreGetValue (guiInstSamplesViewModel gui) i
            lst <- P.zip [0..] <$> listStoreToList (guiInstSamplesViewModel gui)
            let filech = afFileChannel val
                indexes = P.map fst . P.filter ((== filech) . afFileChannel . snd) $ lst
            mapM_ (\x -> treeSelectionSelectPath sel [x]) indexes
        _ -> return ()



initChannelCombo :: ComboBox -> IO ()
initChannelCombo cb = do
    void $ comboBoxSetModelText cb
    void $ mapM (comboBoxAppendText cb) chanList


changeChannel :: InstrumentPage -> IO ()
changeChannel gui = do
    sel <- treeViewGetSelection (guiInstSamplesView gui)
    s <- treeSelectionGetSelectedRows sel
    case s of
        [] -> return ()
        lst -> do
            chan' <- comboBoxGetActiveText (guiComboChannel gui)
            case chan' of
                Nothing -> return ()
                Just chan -> do
                    let modif [] = return ()
                        modif (x:_) = do
                            val <- listStoreGetValue ls x
                            listStoreSetValue ls x (val {afChannel = chan})
                        ls = guiInstSamplesViewModel gui

                    mapM_ modif lst

                    afs <- listStoreToList ls
                    updateHitSample gui hsReplaceSamples afs




getSelectedAudioFiles :: InstrumentPage -> IO [AudioFile]
getSelectedAudioFiles gui = do
    sel <- treeViewGetSelection (guiInstSamplesView gui)
    rows <- treeSelectionGetSelectedRows sel
    let ls = guiInstSamplesViewModel gui
    mapM (listStoreGetValue ls) (P.concat rows)



addNewHitSample :: InstrumentPage -> [AudioFile] -> IO ()
addNewHitSample gui afs = do
    instName <- entryGetText (guiEntryName gui)
    j <- listStoreGetSize (guiInstHitViewModel gui)
    let defSample x = HitSample (smplName x) (fromIntegral x) afs
        smplName x = instName `append` "-" `append` pack (show x)
    void $ listStoreAppend (guiInstHitViewModel gui) (defSample (j + 1))



toNewHitSample :: InstrumentPage -> IO ()
toNewHitSample gui = do
    afs <- getSelectedAudioFiles gui

    -- get the selected hit sample and remove the audio samples
    -- then get the drop destination hit sample and add the samples
    sel <- treeViewGetSelection (guiInstHitView gui)
    srcx' <- treeSelectionGetSelectedRows sel
    case srcx' of
        ((srcx:_):_) -> do
            let ls = guiInstHitViewModel gui
            src <- listStoreGetValue ls srcx
            listStoreSetValue ls srcx (hsRemoveSamples src afs)
            addNewHitSample gui afs

        _ -> return ()
