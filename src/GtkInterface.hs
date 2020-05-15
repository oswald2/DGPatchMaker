{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module GtkInterface where


import           Control.Monad                 as M
                                                ( void )
import           Control.Monad.IO.Class         ( liftIO )

import           Graphics.UI.Gtk

import           Data.Text                     as T
import           Data.IORef
import           Data.Vector                   as V

import           Gtk.MainWindow
import           Gtk.InstrumentFrame
import           Gtk.Drumkit
import           Gtk.FileHandlingDialog
import           Gtk.ErrorDialog

import           Gtk.DGPatchMakerBuilder




initMainWindow :: IO MainWindow
initMainWindow = do
    void initGUI
    -- Create the builder, and load the UI file
    builder <- builderNew

    --builderAddFromFile builder "DGPatchMaker.glade"
    builderAddFromString builder builderFileAsString

    -- Retrieve some objects from the UI
    window <- builderGetObject builder castToWindow ("mainWindow" :: Text)

    windowMaximize window

    set window [windowTitle := ("DrumgGizmo Patch Maker" :: Text)]

    notebook <- builderGetObject builder castToNotebook ("notebookMain" :: Text)
    itemQuit <- builderGetObject builder castToMenuItem ("menuitemQuit" :: Text)
    notebookInstruments <- builderGetObject builder
                                            castToNotebook
                                            ("notebookInstruments" :: Text)

    buttonNewInstrument <- builderGetObject builder
                                            castToButton
                                            ("buttonNewInstrument" :: Text)
    buttonRemoveInstrument <- builderGetObject builder
                                            castToButton
                                            ("buttonRemoveInstrument" :: Text)
    entryBaseDirectory <- builderGetObject builder
                                           castToEntry
                                           ("entryBaseDirectory" :: Text)
    entrySamplesDir <- builderGetObject builder
                                        castToEntry
                                        ("entrySamplesDirectory" :: Text)
    entryExportDir <- builderGetObject builder
                                        castToEntry
                                        ("entryExportDirectory" :: Text)

    progress <- builderGetObject builder
                                 castToProgressBar
                                 ("progressbar" :: Text)

    combo <- builderGetObject builder castToComboBox ("comboboxParser" :: Text)

    fhDialog    <- initFileHandlingDialog builder
    errDiag     <- initErrorDialog builder

    instPages   <- newIORef (V.empty)

    -- initialise the drumkit page
    drumkitPage <- initDrumkitPage window
                                   builder
                                   notebookInstruments
                                   progress
                                   combo
                                   entryBaseDirectory
                                   entrySamplesDir
                                   entryExportDir
                                   instPages
                                   fhDialog

    let gui = MainWindow
            { guiWindow              = window
            , guiNotebook            = notebook
            , guiNotebookInstruments = notebookInstruments
            , guiInstrumentPages     = instPages
            , guiProgress            = progress
            , guiDrumkitPage         = drumkitPage
            }

    --insertInstrumentPage inst

    void $ on buttonNewInstrument buttonActivated $ do
        let name = "New Instrument" :: Text
        ins <- instrumentPageNew window
                                 notebookInstruments
                                 entryExportDir
                                 entrySamplesDir
                                 combo
                                 instPages
                                 fhDialog
                                 errDiag
                                 (setDkSampleRate drumkitPage)
        i <- notebookAppendPage notebookInstruments
                                (instrumentPageGetMainBox ins)
                                name
        instrumentPageInsert ins
        notebookSetCurrentPage notebookInstruments i
        instrumentPageSetInstrumentName ins name

    void $ on buttonRemoveInstrument buttonActivated $ do 
        i <- notebookGetCurrentPage notebookInstruments
        modifyIORef' instPages $ V.ifilter (\ix _ -> ix /= i)
        notebookRemovePage notebookInstruments i   


    -- set termination
    void $ window `on` deleteEvent $ liftIO quit
    void $ on itemQuit menuItemActivate (void quit)


    -- setup about dialog
    aboutDialog <- aboutDialogNew
    set
        aboutDialog
        [ aboutDialogProgramName := ("DrumGizmo Patch Maker" :: Text)
        , aboutDialogVersion := ("V0.8" :: Text)
        , aboutDialogCopyright := ("(C) by Michael Oswald" :: Text)
        , aboutDialogComments
            := ("A tool for creating patches for the drumgizmo plugin\n\n" :: Text
               )
        , aboutDialogAuthors := [("Michael Oswald" :: Text)]
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





