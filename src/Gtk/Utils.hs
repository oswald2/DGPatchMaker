{-# LANGUAGE OverloadedStrings, BangPatterns, NoImplicitPrelude #-}
module Gtk.Utils where

import           ClassyPrelude

import           Data.Char
import qualified Data.Vector                   as V

import           Graphics.UI.Gtk
import           Data.Text                     as T
                                         hiding ( map )


displayErrorBox :: Window -> Text -> IO ()
displayErrorBox parentWindow txt = do
  dialog <- messageDialogNew (Just parentWindow)
                             [DialogDestroyWithParent]
                             MessageError
                             ButtonsClose
                             (cropText txt)
  _ <- dialogRun dialog
  widgetHide dialog
  return ()

displayInfoBox :: Window -> Text -> IO ()
displayInfoBox parentWindow txt = do
  dialog <- messageDialogNew (Just parentWindow)
                             [DialogDestroyWithParent]
                             MessageInfo
                             ButtonsClose
                             (cropText txt)
  _ <- dialogRun dialog
  widgetHide dialog
  return ()


cropText :: Text -> Text
cropText txt =
  let len = 1024
  in  if T.length txt > len then T.take len txt `append` "..." else txt


clearNotebook :: Notebook -> IO ()
clearNotebook nb = do
  n <- notebookGetNPages nb
  forM_ [0 .. n] (notebookRemovePage nb)


setListStoreTo :: ListStore a -> [a] -> IO ()
setListStoreTo ls xs = do
  listStoreClear ls
  mapM_ (void . listStoreAppend ls) xs

setListStoreToVec :: ListStore a -> Vector a -> IO ()
setListStoreToVec ls xs = do
  listStoreClear ls
  V.mapM_ (void . listStoreAppend ls) xs




listStoreMap :: ListStore a -> (a -> a) -> IO ()
listStoreMap ls f = do
  xs <- listStoreToList ls
  let newxs = map f xs
  setListStoreTo ls newxs

listStoreIMap :: ListStore a -> (Int -> a -> a) -> IO ()
listStoreIMap ls f = do
    len <- listStoreGetSize ls
    let go i 
            | i >= len = return () 
            | otherwise = do 
                val <- listStoreGetValue ls i 
                listStoreSetValue ls i (f i val)

    go 0                 


textViewGetText :: TextView -> IO Text
textViewGetText tv = do
  buffer       <- textViewGetBuffer tv
  (start, end) <- textBufferGetBounds buffer
  textBufferGetText buffer start end False


activateRow :: TreeView -> Int -> IO ()
activateRow tv idx = do
  Just col <- treeViewGetColumn tv 0
  treeViewRowActivated tv [idx] col


isLeftChannel :: Text -> Bool
isLeftChannel x | T.last x == 'L' = True
                | isDigit (T.last x) && (T.last (T.dropEnd 1 x)) == 'L' = True
                | otherwise       = False

isRightChannel :: Text -> Bool
isRightChannel x | T.last x == 'R' = True
                 | isDigit (T.last x) && (T.last (T.dropEnd 1 x)) == 'R' = True
                 | otherwise       = False

