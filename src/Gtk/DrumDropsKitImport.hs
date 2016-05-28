{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.DrumDropsKitImport
where


import Control.Monad (filterM)

import Graphics.UI.Gtk

import Data.Text as T
--import Data.IORef
--import Data.Vector as V
import Prelude as P

import System.Directory
import System.FilePath

import Gtk.MainWindow



importDrumDropsDrumKit :: MainWindow a -> IO ()
importDrumDropsDrumKit gui = do
    b <- entryGetText (guiBaseDir gui) :: IO FilePath
    case b of
        "" -> displayErrorBox gui "Basedir must be set!"
        basedir -> do
            samplesDir <- entryGetText (guiSamplesDir gui)

            dirs' <- getDirectoriesToImport samplesDir
            case dirs' of
                Left err -> displayErrorBox gui err
                Right dirs -> do

                    return ()



getDirectoriesToImport :: FilePath -> IO (Either Text [FilePath])
getDirectoriesToImport samplesDir = do
    is <- doesDirectoryExist samplesDir
    if is
        then do
            cont <- getDirsRecursive samplesDir
            return (Right cont)
        else do
            return (Left (pack samplesDir `append` " is not a directory"))
    where
        getDirsRecursive path acc = do
            cont' <- getDirectoryContents path
            let cont'' = P.map (samplesDir </>) $ P.filter (\x -> not (elem x [".", ".."])) cont'
            cont <- filterM doesDirectoryExist cont''

            mapM rec cont
            where
                rec dir = do
                    cont' <- getDirectoryContents path
                    let cont'' = P.map (dir </>) $ P.filter (\x -> not (elem x [".", ".."])) cont'
                    cont <- filterM doesDirectoryExist cont''

