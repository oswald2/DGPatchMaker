{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.MultiVelocity
where



import System.FilePath


import Data.Text as T



getFiles :: FilePath -> IO (Either Text [FilePath])
getFiles path =
    if doesDirectoryExist path
        then do
            cont' <- getDirectoryContents
            let cont = filter (not elem [".", ".."]) cont'
            return (Right cont)
        else do
            return (Left (pack path T.++ " is not a directory"))


getVelocity :: FilePath -> Int
getVelocity file = 0
    where
        filename = takeFileName file
