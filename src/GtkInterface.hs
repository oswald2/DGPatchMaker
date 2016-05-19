{-# LANGUAGE OverloadedStrings #-}
module GtkInterface
where


import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Data.Text

import Gtk.InstrumentFrame


data MainWindow = MainWindow {
    guiWindow :: Window,
    guiNotebook :: Notebook,
    guiNotebookInstruments :: Notebook
}


initMainWindow :: IO MainWindow
initMainWindow = do
    initGUI
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

    inst <- newInstrumentPage

    notebookAppendPage notebookInstruments (getMainBox inst) ("Instrument 1" :: Text)

    let gui = MainWindow {
        guiWindow = window,
        guiNotebook = notebook,
        guiNotebookInstruments = notebookInstruments
        }

    -- set termination
    window `on` deleteEvent $ liftIO quit
    on itemQuit menuItemActivate (void quit)


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

gtkInterfaceMainLoop :: IO ()
gtkInterfaceMainLoop = do
    mainGUI


quit = do
    mainQuit
    return False
