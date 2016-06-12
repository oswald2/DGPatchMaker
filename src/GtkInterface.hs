{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module GtkInterface
where


import Control.Monad  as M (void)
import Control.Monad.IO.Class (liftIO)

import Graphics.UI.Gtk

import Data.Text as T
import Data.IORef
import Data.Vector as V

import Gtk.MainWindow
import Gtk.InstrumentFrame
import Gtk.Drumkit


initMainWindow :: IO MainWindow
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

    buttonNewInstrument <- builderGetObject builder castToButton ("buttonNewInstrument" :: Text)
    entryBaseDirectory <- builderGetObject builder castToEntry ("entryBaseDirectory" :: Text)
    entrySamplesDir <- builderGetObject builder castToEntry ("entrySamplesDirectory" :: Text)

    progress <- builderGetObject builder castToProgressBar ("progressbar" :: Text)

    instPages <- newIORef (V.empty)

    -- initialise the drumkit page
    drumkitPage <- initDrumkitPage window builder notebookInstruments progress entryBaseDirectory entrySamplesDir instPages

    let gui = MainWindow {
        guiWindow = window,
        guiNotebook = notebook,
        guiNotebookInstruments = notebookInstruments,
        guiInstrumentPages = instPages,
        guiProgress = progress,
        guiDrumkitPage = drumkitPage
        }

    --insertInstrumentPage inst

    void $ on buttonNewInstrument buttonActivated $ do
        ins <- newInstrumentPage window notebookInstruments entryBaseDirectory entrySamplesDir instPages
        i <- notebookAppendPage notebookInstruments (getMainBox ins) ("Instrument 1" :: Text)
        insertInstrumentPage ins
        notebookSetCurrentPage notebookInstruments i

    -- set termination
    void $ window `on` deleteEvent $ liftIO quit
    void $ on itemQuit menuItemActivate (void quit)


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

gtkInterfaceMainLoop :: MainWindow -> IO ()
gtkInterfaceMainLoop _ = do
    mainGUI


quit :: IO Bool
quit = do
    mainQuit
    return False





