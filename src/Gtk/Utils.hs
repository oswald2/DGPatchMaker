module Gtk.Utils


where


import Control.Monad (forM_)

import Graphics.UI.Gtk
import Data.Text


displayErrorBox :: Window -> Text -> IO ()
displayErrorBox parentWindow txt = do
    dialog <- messageDialogNew (Just parentWindow) [DialogDestroyWithParent] MessageError ButtonsClose txt
    _ <- dialogRun dialog
    widgetHide dialog
    return ()

displayInfoBox :: Window -> Text -> IO ()
displayInfoBox parentWindow txt = do
    dialog <- messageDialogNew (Just parentWindow) [DialogDestroyWithParent] MessageInfo ButtonsClose txt
    _ <- dialogRun dialog
    widgetHide dialog
    return ()

clearNotebook :: Notebook -> IO ()
clearNotebook nb = do
    n <- notebookGetNPages nb
    forM_ [0..n] (notebookRemovePage nb)


setListStoreTo :: ListStore a -> [a] -> IO ()
setListStoreTo ls xs = do
    listStoreClear ls
    mapM_ (listStoreAppend ls) xs

