{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.DrumDrops.Utils

where


import Prelude as P

import System.FilePath
import System.Directory

import Data.Text as T


getFiles :: FilePath -> IO (Either Text [FilePath])
getFiles path = do
    is <- doesDirectoryExist path
    if is
        then do
            cont' <- getDirectoryContents path
            let cont = P.filter (\x -> not (elem x [".", ".."])) cont'
            return (Right cont)
        else do
            return (Left (pack path `T.append` " is not a directory"))


