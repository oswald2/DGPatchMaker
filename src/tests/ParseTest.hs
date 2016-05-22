{-# LANGUAGE OverloadedStrings #-}
module Main

where

import System.FilePath

import Data.DrumDrops.Types
import Data.DrumDrops.Utils

import Data.Text as T
import Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
--import Data.List (sort)
--import Data.Either

import Data.Drumgizmo
import Data.Export


str1 :: String
str1 = "MPEX_KICK_EQCL_HT_005_V1_RR1.wav"

path :: FilePath
path = "/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Kontakt Pack Samples/Kontakt Pack Samples/Mapex Kick Drum/EQ Head"

basepath :: FilePath
basepath = "/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Kontakt Pack Samples"


main :: IO ()
main = do

    files <- getSamples path
    case files of
        Left err -> T.putStrLn err
        Right groups -> do
            -- printSampleGroup groups
            let
                dgInstrumentsPath = getInstrumentDir basepath

            dir <- createDrumgizmoDirectories basepath
            case dir of
                Left err -> T.putStrLn err
                Right () -> do

                    -- create the instrument file
                    let content = convertToInstrumentXML (convertSampleGroup basepath groups)
                        filename = dgInstrumentsPath </> T.unpack (sgInstName groups) <.> "xml"
                    B.writeFile filename content

                    -- create the drumkit




printSample :: Sample -> IO ()
printSample x = Prelude.putStrLn (show x)


printSampleGroup :: SampleGroup -> IO ()
printSampleGroup x = do
    T.putStrLn ("Path: " `T.append` T.pack (sgPath x))
    T.putStrLn ("Instrument: " `T.append` sgInstName x)
    mapM_ printGroup (sgGroups x)

printGroup :: VelocityGroup -> IO ()
printGroup x = do
    T.putStrLn ("Group: Velocity: " `append` pack (show (vgVelocity x)) `append` " Round Robin: " `append` pack (show (vgRR x)))
    mapM_ printSample (vgSamples x)
