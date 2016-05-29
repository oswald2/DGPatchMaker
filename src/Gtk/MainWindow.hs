module Gtk.MainWindow
where


import Graphics.UI.Gtk
import Data.Text
import Data.IORef
import Data.Vector


black :: Color
black = Color 0 0 0

white :: Color
white = Color 0xffff 0xffff 0xffff

green :: Color
green = Color 0 0xffff 0

yellow :: Color
yellow = Color 0xffff 0xffff 0

red :: Color
red = Color 0xffff 0 0

paleYellow :: Color
paleYellow = Color 0xffff 0xffff (102*256)


data MainWindow a = MainWindow {
    guiWindow :: Window,
    guiNotebook :: Notebook,
    guiNotebookInstruments :: Notebook,
    guiBaseDir :: Entry,
    guiSamplesDir :: Entry,
    guiInstrumentPages :: IORef (Vector a),
    guiProgress :: ProgressBar
}

displayErrorBox :: MainWindow a -> Text -> IO ()
displayErrorBox parentWindow txt = do
    dialog <- messageDialogNew (Just (guiWindow parentWindow)) [DialogDestroyWithParent] MessageError ButtonsClose txt
    _ <- dialogRun dialog
    widgetHide dialog
    return ()

displayInfoBox :: MainWindow a -> Text -> IO ()
displayInfoBox parentWindow txt = do
    dialog <- messageDialogNew (Just (guiWindow parentWindow)) [DialogDestroyWithParent] MessageInfo ButtonsClose txt
    _ <- dialogRun dialog
    widgetHide dialog
    return ()
