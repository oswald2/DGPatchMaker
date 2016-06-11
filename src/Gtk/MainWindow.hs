module Gtk.MainWindow
where


import Graphics.UI.Gtk
import Data.Text
import Data.IORef
import Data.Vector
import Gtk.Drumkit

import Gtk.InstrumentFrame


data MainWindow = MainWindow {
    guiWindow :: Window,
    guiNotebook :: Notebook,
    guiNotebookInstruments :: Notebook,
    --guiInstrumentPages :: IORef (Vector InstrumentFrame),
    guiProgress :: ProgressBar,
    guiDrumkitPage :: DrumkitPage
}



