{-# LANGUAGE OverloadedStrings, BangPatterns, NoImplicitPrelude #-}
module Gtk.Utils


where

import ClassyPrelude


import Graphics.UI.Gtk
import Data.Text as T


displayErrorBox :: Window -> Text -> IO ()
displayErrorBox parentWindow txt = do
    dialog <- messageDialogNew (Just parentWindow) [DialogDestroyWithParent] MessageError ButtonsClose (cropText txt)
    _ <- dialogRun dialog
    widgetHide dialog
    return ()

displayInfoBox :: Window -> Text -> IO ()
displayInfoBox parentWindow txt = do
    dialog <- messageDialogNew (Just parentWindow) [DialogDestroyWithParent] MessageInfo ButtonsClose (cropText txt)
    _ <- dialogRun dialog
    widgetHide dialog
    return ()


cropText :: Text -> Text
cropText txt =
    if T.length txt > 400 then T.take 400 txt `append` "..." else txt


clearNotebook :: Notebook -> IO ()
clearNotebook nb = do
    n <- notebookGetNPages nb
    forM_ [0..n] (notebookRemovePage nb)


setListStoreTo :: ListStore a -> [a] -> IO ()
setListStoreTo ls xs = do
    listStoreClear ls
    mapM_ (void . listStoreAppend ls) xs

