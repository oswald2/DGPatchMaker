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
import qualified Data.DrumDrops.MapexKitParser as MP
import qualified Data.DrumDrops.VintageFolkParser as VFP

import Data.Types

import Sound.File.Sndfile (getFileInfo, Info(..))

import Text.Parsec (ParseError)



data ParserType =
    MapexParser
    | VintageFolkParser
    deriving (Enum, Eq, Ord, Show, Read)




importInstrument :: ParserType -> FilePath -> FilePath -> FilePath -> IO (Either Text InstrumentFile)
importInstrument parserType basepath samplesPath path = do

    putStrLn $ "Importing Instrument from: " ++ path

    w <- getSamples parserType samplesPath path
    case w of
        Left err -> return (Left err)
        Right wavFiles -> return (Right (convertSampleGroup basepath wavFiles))



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

getSamples :: ParserType -> FilePath -> FilePath -> IO (Either Text SampleGroup)
getSamples parserType samplesDir path = do
    fs <- getFiles path
    case fs of
        Left err -> return (Left err)
        Right cont -> do
            -- get audio information out of the wav files
            let proc c = do
                    info <- getFileInfo (path </> c)
                    return (getSampleFromFileName parserType c (channels info))

            res <- mapM proc $ filter (\x -> takeExtension x == ".wav" || takeExtension x == ".WAV") cont

            let
                spls = rights res
                errors = lefts res

            if P.null errors
                then do
                    let gr = SampleGroup path (pathToInstrument samplesDir path) inst vgs
                        inst = vgInstrument (head vgs)
                        vgs = getVelocityGroups spls
                    return (Right gr)
                else do
                    let err = "Failed parsing: " `append` pack (show errors)
                    return (Left err)


pathToInstrument :: FilePath -> FilePath -> Text
pathToInstrument sampleDir path' =
    let path = makeRelative sampleDir path'
        fs = splitDirectories path
        ps = P.concat fs
        inst = P.filter (not . isSpace) ps
    in
    pack inst



getSampleFromFileName :: ParserType -> FilePath -> Int -> Either ParseError Sample
getSampleFromFileName MapexParser = MP.getSampleFromFileName
getSampleFromFileName VintageFolkParser = VFP.getSampleFromFileName

