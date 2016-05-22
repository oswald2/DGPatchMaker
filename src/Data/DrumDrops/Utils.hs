{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.DrumDrops.Utils

where


import Prelude as P

import System.Directory
import System.FilePath
import Data.List as L (sort, groupBy)
import Data.Text (Text, pack, append)

import Data.Either
import Data.Char (isSpace)
import Data.DrumDrops.Types

import Sound.File.Sndfile (getFileInfo, Info(..))


getFiles :: FilePath -> IO (Either Text [FilePath])
getFiles path = do
    is <- doesDirectoryExist path
    if is
        then do
            cont' <- getDirectoryContents path
            let cont = P.filter (\x -> not (elem x [".", ".."])) cont'
            return (Right cont)
        else do
            return (Left (pack path `append` " is not a directory"))


getVelocityGroups :: [Sample] -> [VelocityGroup]
getVelocityGroups ss =
    let gs = (L.groupBy velocityGroup . sort) ss
        crV x =
            let first = head x in
            VelocityGroup (fromIntegral (saVelocity first)) (saRound first) (saInstrument first) x
    in
    map crV gs

getSamples :: FilePath -> IO (Either Text SampleGroup)
getSamples path = do
    fs <- getFiles path
    case fs of
        Left err -> return (Left err)
        Right cont -> do
            -- get audio information out of the wav files
            let proc c = do
                    info <- getFileInfo (path </> c)
                    return (getSampleFromFileName c (channels info))

            res <- mapM proc cont

            let
                spls = rights res
                errors = lefts res

            if P.null errors
                then do
                    let gr = SampleGroup path (pathToInstrument path) inst vgs
                        inst = vgInstrument (head vgs)
                        vgs = getVelocityGroups spls
                    return (Right gr)
                else do
                    let err = "Failed parsing: " `append` pack (show errors)
                    return (Left err)


pathToInstrument :: FilePath -> Text
pathToInstrument path =
    let fs = splitDirectories path
        ps = P.concat (takeLastTwo fs)
        inst = P.filter (not . isSpace) ps

        takeLastTwo (x:y:[]) = [x, y]
        takeLastTwo (x:[]) = [x]
        takeLastTwo [] = []
        takeLastTwo (_:xs) = takeLastTwo xs
    in
    pack inst
