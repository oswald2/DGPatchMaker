{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.InstrumentFrame
    (
    InstrumentPage
    ,newInstrumentPage
    ,getMainBox
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Prelude as P

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Builder

import Data.Text as T
--import qualified Data.Text.Lazy as TL
--import Data.Text.Format
import qualified Data.ByteString.Lazy as B

import Data.Types
import Data.IORef
import Data.Checkers
import Data.Drumgizmo
import Data.Export
import Data.Maybe
import Data.Char (isSpace)

import Data.DrumDrops.Utils

import System.FilePath

import Gtk.MainWindow


data InstrumentPage = InstrumentPage {
    guiInstMainBox :: Box,
    guiMainWindow :: MainWindow InstrumentPage,
    guiInstHitView :: TreeView,
    guiInstHitViewModel :: ListStore HitSample,
    guiRendererHP :: CellRendererText,
    guiRendererChan :: CellRendererText,
    guiRendererFileChan :: CellRendererText,
    guiInstSamplesView :: TreeView,
    guiInstSamplesViewModel :: ListStore AudioFile,
    guiInstFile :: IORef (Maybe InstrumentFile),
    guiEntryVersion :: Entry,
    guiEntryName :: Entry,
    guiEntryType :: Entry,
    guiAudioSamplesMenu :: Menu
    }




newInstrumentPage :: MainWindow InstrumentPage -> IO InstrumentPage
newInstrumentPage parentWindow = do
    -- Create the builder, and load the UI file
    builder <- builderNew

    builderAddFromFile builder "InstrumentPage.glade"
    --builderAddFromString builder builderFileAsString

    -- Retrieve some objects from the UI
    mainBox <- builderGetObject builder castToBox ("mainBox" :: Text)
    treeviewHit <- builderGetObject builder castToTreeView ("treeviewHit" :: Text)
    treeviewSamples <- builderGetObject builder castToTreeView ("treeviewSamples" :: Text)

    buttonOpenSamples <- builderGetObject builder castToButton ("buttonOpenSamples" :: Text)
    buttonImportInstrument <- builderGetObject builder castToButton ("buttonImportInstrument" :: Text)
    buttonExportInstrument <- builderGetObject builder castToButton ("buttonExportInstrument" :: Text)

    entryVersion <- builderGetObject builder castToEntry ("entryVersion" :: Text)
    entryName <- builderGetObject builder castToEntry ("entryName" :: Text)
    entryType <- builderGetObject builder castToEntry ("entryType" :: Text)

    popUp <- builderGetObject builder castToMenu ("menuAudioSamples" :: Text)
    menuAddSamples <- builderGetObject builder castToMenuItem ("menuitemAdd" :: Text)
    menuRemoveSample <- builderGetObject builder castToMenuItem ("menuitemRemove" :: Text)

    hsls <- listStoreNew []
    rendererHP <- initTreeViewHit treeviewHit hsls

    sals <- listStoreNew []
    (rendChan, rendFileChan) <- initTreeViewSamples treeviewSamples sals

    ifr <- newIORef Nothing

    let gui = InstrumentPage {
        guiInstMainBox = mainBox,
        guiMainWindow = parentWindow,
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
        guiRendererFileChan = rendFileChan
        }

    -- setup the callback for the import button
    void $ on buttonImportInstrument buttonActivated (importDrumDropsInstrument gui)
    void $ on buttonExportInstrument buttonActivated (exportInstrument gui)

    void $ on entryName entryActivated (validateName gui)
    void $ on entryName focusOutEvent (liftIO (validateName gui) >> return False)
    void $ on entryType entryActivated (validateType gui)
    void $ on entryType focusOutEvent (liftIO (validateType gui) >> return False)

    void $ on menuAddSamples menuItemActivate (addSamples treeviewSamples sals)
    void $ on buttonOpenSamples buttonActivated (addSamples treeviewSamples sals)
    void $ on menuRemoveSample menuItemActivate (removeSamples treeviewSamples sals)


    -- setup the local callbacks for the treeviews
    setupCallbacks gui

    return gui


importDrumDropsInstrument :: InstrumentPage -> IO ()
importDrumDropsInstrument instPage = do
    let mainWindow = guiMainWindow instPage
        parentWindow = guiWindow mainWindow

    dialog <- fileChooserDialogNew
                (Just $ ("Import DrumDrops Instrument from Path" :: Text))             --dialog title
                (Just parentWindow)                     --the parent window
                FileChooserActionSelectFolder                         --the kind of dialog we want
                [("gtk-cancel"                                --The buttons to display
                 ,ResponseCancel)
                 ,("gtk-open"
                 , ResponseAccept)]

    basedir <- entryGetText (guiBaseDir mainWindow)
    sampleDir <- entryGetText (guiSamplesDir mainWindow)
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

                    result <- importInstrument basedir sampleDir instrumentDir
                    case result of
                        Right instrumentFile -> do
                            setInstrumentFile instPage instrumentFile
                            listStoreClear (guiInstSamplesViewModel instPage)
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



getMainBox :: InstrumentPage -> Box
getMainBox = guiInstMainBox


initTreeViewHit :: TreeView -> ListStore HitSample -> IO CellRendererText
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

    return rendererHP






initTreeViewSamples :: TreeView -> ListStore AudioFile -> IO (CellRendererText, CellRendererText)
initTreeViewSamples tv ls = do
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

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `T.isPrefixOf` toLower (pack (afPath row))

    return (renderer1, renderer3)



setCurrentNotebookLabel :: InstrumentPage -> Text -> IO ()
setCurrentNotebookLabel instPage name = do
    let notebook = guiNotebookInstruments (guiMainWindow instPage)
    currentPage <- notebookGetCurrentPage notebook
    page <- notebookGetNthPage notebook currentPage
    case page of
        Just p -> notebookSetTabLabelText notebook p name
        Nothing -> return ()


setInstrumentFile :: InstrumentPage -> InstrumentFile -> IO ()
setInstrumentFile instPage instrumentFile = do
    let ref = guiInstFile instPage
    writeIORef ref (Just instrumentFile)

    setCurrentNotebookLabel instPage (ifName instrumentFile)

    entrySetText (guiEntryVersion instPage) (ifVersion instrumentFile)
    entrySetText (guiEntryName instPage) (ifName instrumentFile)
    entrySetText (guiEntryType instPage) ((pack.show.ifType) instrumentFile)

    let model = guiInstHitViewModel instPage

    setListStoreTo model (ifSamples instrumentFile)

    return ()


setListStoreTo :: ListStore a -> [a] -> IO ()
setListStoreTo ls xs = do
    listStoreClear ls
    mapM_ (listStoreAppend ls) xs


setupCallbacks :: InstrumentPage -> IO ()
setupCallbacks instPage = do
    let hitview = guiInstHitView instPage
        hitviewModel = guiInstHitViewModel instPage

    void $ on hitview rowActivated $ \(i:_) _ -> do
        !row <- listStoreGetValue hitviewModel i
        setListStoreTo (guiInstSamplesViewModel instPage) (hsSamples row)

        return ()

    void $ on (guiInstSamplesView instPage) buttonPressEvent $ do
        bt <- eventButton
        case bt of
            RightButton -> do
                liftIO $ menuPopup (guiAudioSamplesMenu instPage) Nothing
                return True
            _ -> return False


    void $ on (guiRendererHP instPage) edited $ \[i] str -> do
        val <- listStoreGetValue (guiInstHitViewModel instPage) i
        let res = checkFloat str
        case res of
            Left err -> displayErrorBox (guiMainWindow instPage) err
            Right x -> do
                -- set the GTK list store to the new value
                listStoreSetValue (guiInstHitViewModel instPage) i (val {hsPower = x})

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




addSamples :: TreeView -> ListStore AudioFile -> IO ()
addSamples _ _ = return ()


removeSamples :: TreeView -> ListStore AudioFile -> IO ()
removeSamples _ _ = return ()


getInstrumentFromGUI :: InstrumentPage -> IO (Either Text InstrumentFile)
getInstrumentFromGUI instPage = do
    name <- entryGetText (guiEntryName instPage)
    t <- entryGetText (guiEntryType instPage)
    samples <- listStoreToList (guiInstHitViewModel instPage)
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
    i <- getInstrumentFromGUI instPage
    case i of
        Left err -> displayErrorBox (guiMainWindow instPage) err
        Right instrumentFile -> do
            storeInstrument instPage instrumentFile

            basepath <- entryGetText (guiBaseDir (guiMainWindow instPage))

            let
                dgInstrumentsPath = getInstrumentDir basepath
                content = convertToInstrumentXML instrumentFile
                filename = dgInstrumentsPath </> T.unpack (ifName instrumentFile) <.> "xml"
            B.writeFile filename content
            displayInfoBox (guiMainWindow instPage) ("Successfully exported instrument (" `append` pack filename `append` ")")


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
        Nothing -> Left ("Illegal input: " `append` input)


validateType :: InstrumentPage -> IO ()
validateType instPage = do
    t <- entryGetText (guiEntryType instPage)
    let r = validate t
    case r of
        Left err -> displayErrorBox (guiMainWindow instPage) err
        Right x -> do
            iF <- readIORef (guiInstFile instPage)
            case iF of
                Nothing -> return ()
                Just instF -> do
                    let !iF' = instF {ifType = x}
                    writeIORef (guiInstFile instPage) (Just iF')
