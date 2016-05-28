{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module GtkInterface
where


import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Graphics.UI.Gtk

import Data.Text
import Data.IORef
import Data.Vector as V

import Gtk.MainWindow
import Gtk.InstrumentFrame
import Gtk.DrumDropsKitImport


initMainWindow :: IO (MainWindow InstrumentPage)
initMainWindow = do
    void initGUI
    -- Create the builder, and load the UI file
    builder <- builderNew

    builderAddFromFile builder "DGPatchMaker.glade"
    --builderAddFromString builder builderFileAsString

    -- Retrieve some objects from the UI
    window <- builderGetObject builder castToWindow ("mainWindow" :: Text)

    set window [windowTitle := ("DrumgGizmo Patch Maker" :: Text)]

    notebook <- builderGetObject builder castToNotebook ("notebookMain" :: Text)
    itemQuit <- builderGetObject builder castToMenuItem ("menuitemQuit" :: Text)
    notebookInstruments <- builderGetObject builder castToNotebook ("notebookInstruments" :: Text)

    buttonImportDrumkit <- builderGetObject builder castToButton ("buttonImportDrumkit" :: Text)
    buttonNewInstrument <- builderGetObject builder castToButton ("buttonNewInstrument" :: Text)
    buttonSetBaseDir <- builderGetObject builder castToButton ("buttonSetBaseDir" :: Text)
    buttonSetSamplesDir <- builderGetObject builder castToButton ("buttonSetSamplesDir" :: Text)
    entryBaseDirectory <- builderGetObject builder castToEntry ("entryBaseDirectory" :: Text)
    entrySamplesDir <- builderGetObject builder castToEntry ("entrySamplesDirectory" :: Text)

    entrySetText entryBaseDirectory ("/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Kontakt Pack Samples" :: FilePath)
    entrySetText entrySamplesDir ("/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Kontakt Pack Samples/Kontakt Pack Samples" :: FilePath)

    instPages <- newIORef (V.empty)

    let gui = MainWindow {
        guiWindow = window,
        guiNotebook = notebook,
        guiNotebookInstruments = notebookInstruments,
        guiBaseDir = entryBaseDirectory,
        guiSamplesDir = entrySamplesDir,
        guiInstrumentPages = instPages
        }

    inst <- newInstrumentPage gui
    void $ notebookAppendPage notebookInstruments (getMainBox inst) ("Instrument 1" :: Text)
    insertInstrumentPage gui inst

    void $ on buttonNewInstrument buttonActivated $ do
        ins <- newInstrumentPage gui
        i <- notebookAppendPage notebookInstruments (getMainBox ins) ("Instrument 1" :: Text)
        insertInstrumentPage gui ins
        notebookSetCurrentPage notebookInstruments i

    -- set termination
    void $ window `on` deleteEvent $ liftIO quit
    void $ on itemQuit menuItemActivate (void quit)

    void $ on buttonSetBaseDir buttonActivated $ setBaseDir gui
    void $ on buttonSetSamplesDir buttonActivated $ setSamplesDir gui

    void $ on buttonImportDrumkit buttonActivated $ importDrumDropsDrumKit gui

    -- setup about dialog
    aboutDialog <- aboutDialogNew
    set aboutDialog [aboutDialogProgramName := ("DrumGizmo Patch Maker":: Text),
        aboutDialogVersion := ("V0.1" :: Text),
        aboutDialogCopyright := ("(C) by Michael Oswald" :: Text),
            aboutDialogComments := ("A tool for creating patches for the drumgizmo plugin\n\n" :: Text),
        aboutDialogAuthors := [("Michael Oswald" :: Text)]
        ]
   -- Display the window
    widgetShowAll window

    return gui

gtkInterfaceMainLoop :: MainWindow a -> IO ()
gtkInterfaceMainLoop _ = do
    mainGUI


quit :: IO Bool
quit = do
    mainQuit
    return False


insertInstrumentPage :: MainWindow InstrumentPage -> InstrumentPage -> IO ()
insertInstrumentPage gui instPage = do
    v <- readIORef (guiInstrumentPages gui)
    let !v' = v V.++ V.singleton instPage
    writeIORef (guiInstrumentPages gui) v'


setBaseDir :: MainWindow a -> IO ()
setBaseDir mainWindow = do
    let parentWindow = guiWindow mainWindow
    dialog <- fileChooserDialogNew
              (Just $ ("Set Base Directory for Imports" :: Text))             --dialog title
              (Just parentWindow)                     --the parent window
              FileChooserActionSelectFolder                         --the kind of dialog we want
              [("gtk-cancel"                                --The buttons to display
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]

    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept -> do
            f <- fileChooserGetFilename dialog
            case f of
                Nothing -> return ()
                Just directory -> do
                    entrySetText (guiBaseDir mainWindow) directory
                    return ()
        ResponseCancel -> return ()
        ResponseDeleteEvent -> return ()
        _ -> return ()
    widgetHide dialog


setSamplesDir :: MainWindow a -> IO ()
setSamplesDir mainWindow = do
    let parentWindow = guiWindow mainWindow
    dialog <- fileChooserDialogNew
              (Just $ ("Set Sample Base Directory for Imports" :: Text))             --dialog title
              (Just parentWindow)                     --the parent window
              FileChooserActionSelectFolder                         --the kind of dialog we want
              [("gtk-cancel"                                --The buttons to display
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]

    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept -> do
            f <- fileChooserGetFilename dialog
            case f of
                Nothing -> return ()
                Just directory -> do
                    entrySetText (guiSamplesDir mainWindow) directory
                    return ()
        ResponseCancel -> return ()
        ResponseDeleteEvent -> return ()
        _ -> return ()
    widgetHide dialog
