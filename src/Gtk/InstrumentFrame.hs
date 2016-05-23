{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.InstrumentFrame
    (
    InstrumentPage
    ,newInstrumentPage
    ,getMainBox
    )
where

import Control.Monad (void)
--import Control.Monad.IO.Class (liftIO)

import Prelude as P

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Builder

import Data.Text as T
--import qualified Data.Text.Lazy as TL
--import Data.Text.Format

import Data.Types
import Data.IORef
import Data.Checkers


import Data.DrumDrops.Utils

import Gtk.MainWindow


data InstrumentPage = InstrumentPage {
    guiInstMainBox :: Box,
    guiMainWindow :: MainWindow,
    guiInstHitView :: TreeView,
    guiInstHitViewModel :: ListStore HitSample,
    guiRendererHP :: CellRendererText,
    guiInstSamplesView :: TreeView,
    guiInstSamplesViewModel :: ListStore AudioFile,
    guiInstFile :: IORef (Maybe InstrumentFile),
    guiEntryVersion :: Entry,
    guiEntryName :: Entry,
    guiEntryType :: Entry
    }




newInstrumentPage :: MainWindow -> IO InstrumentPage
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

    hsls <- listStoreNew []
    rendererHP <- cellRendererTextNew
    initTreeViewHit treeviewHit hsls rendererHP

    sals <- listStoreNew []
    initTreeViewSamples treeviewSamples sals

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
        guiRendererHP = rendererHP

        }

    -- setup the callback for the import button
    void $ on buttonImportInstrument buttonActivated (importDrumDropsInstrument parentWindow gui)

    -- setup the local callbacks for the treeviews
    setupCallbacks gui

    return gui


importDrumDropsInstrument :: MainWindow -> InstrumentPage -> IO ()
importDrumDropsInstrument mainWindow instPage = do
    let parentWindow = guiWindow mainWindow

    dialog <- fileChooserDialogNew
                (Just $ ("Import DrumDrops Instrument from Path" :: Text))             --dialog title
                (Just parentWindow)                     --the parent window
                FileChooserActionSelectFolder                         --the kind of dialog we want
                [("gtk-cancel"                                --The buttons to display
                 ,ResponseCancel)
                 ,("gtk-open"
                 , ResponseAccept)]

    basedir <- entryGetText (guiBaseDir mainWindow)
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

                    result <- importInstrument basedir instrumentDir
                    case result of
                        Right instrumentFile -> do
                            setInstrumentFile instPage instrumentFile
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


initTreeViewHit :: TreeView -> ListStore HitSample -> CellRendererText -> IO ()
initTreeViewHit tv ls rendererHP = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Name" :: Text)
    treeViewColumnSetTitle col2 ("Hit Power" :: Text)

    renderer1 <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 rendererHP True

    cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [ cellText := hsName hs]
    cellLayoutSetAttributes col2 rendererHP ls $ \hs -> [ cellText := pack (show (hsPower hs)) ]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `T.isPrefixOf` toLower (hsName row)

    return ()






initTreeViewSamples :: TreeView -> ListStore AudioFile -> IO ()
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

    cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [ cellText := pack (show (afChannel hs))]
    cellLayoutSetAttributes col2 renderer2 ls $ \hs -> [ cellText := afPath hs ]
    cellLayoutSetAttributes col2 renderer2 ls $ \hs -> [ cellText := pack (show (afFileChannel hs)) ]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `T.isPrefixOf` toLower (pack (afPath row))



    return ()


setInstrumentFile :: InstrumentPage -> InstrumentFile -> IO ()
setInstrumentFile instPage instrumentFile = do
    let ref = guiInstFile instPage
    writeIORef ref (Just instrumentFile)

    let notebook = guiNotebookInstruments (guiMainWindow instPage)
    currentPage <- notebookGetCurrentPage notebook
    page <- notebookGetNthPage notebook currentPage
    case page of
        Just p -> notebookSetTabLabelText notebook p (ifName instrumentFile)
        Nothing -> return ()

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

    void $ on (guiRendererHP instPage) edited $ \[i] str -> do
        val <- listStoreGetValue (guiInstHitViewModel instPage) i
        let res = checkFloat str
        case res of
            Left err -> displayErrorBox (guiMainWindow instPage) err
            Right x -> do
                -- set the GTK list store to the new value
                listStoreSetValue (guiInstHitViewModel instPage) i (val {hsPower = x})



    return ()
