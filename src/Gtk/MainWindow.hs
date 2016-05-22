module Gtk.MainWindow
where


import Graphics.UI.Gtk


data MainWindow = MainWindow {
    guiWindow :: Window,
    guiNotebook :: Notebook,
    guiNotebookInstruments :: Notebook,
    guiBaseDir :: Entry
}

