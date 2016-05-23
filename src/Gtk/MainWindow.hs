module Gtk.MainWindow
where


import Graphics.UI.Gtk
import Data.Text

data MainWindow = MainWindow {
    guiWindow :: Window,
    guiNotebook :: Notebook,
    guiNotebookInstruments :: Notebook,
    guiBaseDir :: Entry
}

displayErrorBox :: MainWindow -> Text -> IO ()
displayErrorBox parentWindow txt = do
    dialog <- messageDialogNew (Just (guiWindow parentWindow)) [DialogDestroyWithParent] MessageError ButtonsClose txt
    _ <- dialogRun dialog
    widgetHide dialog
    return ()

displayInfoBox :: MainWindow -> Text -> IO ()
displayInfoBox parentWindow txt = do
    dialog <- messageDialogNew (Just (guiWindow parentWindow)) [DialogDestroyWithParent] MessageInfo ButtonsClose txt
    _ <- dialogRun dialog
    widgetHide dialog
    return ()
