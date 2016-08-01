{-# LANGUAGE OverloadedStrings #-}
module Main

where


import System.FilePath
import System.Environment
import System.Exit

import Data.DrumDrops.Types
import Data.DrumDrops.Utils

import Data.Text as T
import Data.Text.IO as T
--import qualified Data.ByteString.Lazy as B
--import Data.List (sort)
--import Data.Either

import Data.Drumgizmo
import Data.Export

import Data.Types


str1 :: String
str1 = "MPEX_KICK_EQCL_HT_005_V1_RR1.wav"

path :: FilePath
path = "/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Kontakt Pack Samples/Kontakt Pack Samples/Mapex Kick Drum/EQ Head"

basepath :: FilePath
basepath = "/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Kontakt Pack Samples"

samplesPath :: FilePath
samplesPath = "/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Kontakt Pack Samples/Kontakt Pack Samples"

main :: IO ()
main = do
    [parserType, file] <- getArgs
    content <- T.readFile file
    mapM_ (f parserType) (T.lines content)
    where
        f parserType x = do
            T.putStrLn x
            let res = getSampleFromFileName (read parserType) (unpack x) 1
            case res of
                Left err -> do
                    Prelude.putStrLn (show err)
                    exitFailure
                Right sample -> print sample


--printSample :: Sample -> IO ()
--printSample x = Prelude.putStrLn (show x)


--printSampleGroup :: SampleGroup -> IO ()
--printSampleGroup x = do
    --T.putStrLn ("Path: " `T.append` T.pack (sgPath x))
    --T.putStrLn ("Instrument: " `T.append` sgInstName x)
    --mapM_ printGroup (sgGroups x)

--printGroup :: VelocityGroup -> IO ()
--printGroup x = do
    --T.putStrLn ("Group: Velocity: " `append` pack (show (vgVelocity x)) `append` " Round Robin: " `append` pack (show (vgRR x)))
    --mapM_ printSample (vgSamples x)
