{-# LANGUAGE OverloadedStrings #-}
module GtkInterface
where


import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Graphics.UI.Gtk

import Data.Text

import Gtk.MainWindow
import Gtk.InstrumentFrame



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
    buttonSetBaseDir <- builderGetObject builder castToButton ("buttonSetBaseDir" :: Text)
    entryBaseDirectory <- builderGetObject builder castToEntry ("entryBaseDirectory" :: Text)
    entrySetText entryBaseDirectory ("/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Kontakt Pack Samples" :: FilePath)

    let gui = MainWindow {
        guiWindow = window,
        guiNotebook = notebook,
        guiNotebookInstruments = notebookInstruments,
        guiBaseDir = entryBaseDirectory
        }

    inst <- newInstrumentPage gui
    void $ notebookAppendPage notebookInstruments (getMainBox inst) ("Instrument 1" :: Text)

    -- set termination
    void $ window `on` deleteEvent $ liftIO quit
    void $ on itemQuit menuItemActivate (void quit)

    void $ on buttonSetBaseDir buttonActivated $ setBaseDir gui


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


quit :: IO Bool
quit = do
    mainQuit
    return False


setBaseDir :: MainWindow -> IO ()
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
