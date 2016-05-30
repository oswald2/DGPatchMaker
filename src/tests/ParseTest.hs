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

    let
        dgInstrumentsPath = getInstrumentDir basepath
        dgPath = getDrumgizmoDir basepath

    dir <- createDrumgizmoDirectories basepath
    case dir of
        Left err -> T.putStrLn err
        Right () -> do

            -- create the instrument file
            w <- importInstrument basepath samplesPath path
            case w of
                Left err -> T.putStrLn err
                Right instrumentFile -> do
                    let
                        content = convertToInstrumentXML instrumentFile
                        filename = dgInstrumentsPath </> T.unpack (ifName instrumentFile) <.> "xml"
                    B.writeFile filename content

                    -- create the drumkit
                    let drumkit = generateDrumkit "TestKit" "This is a description" [instrumentFile]
                        drumkitCont = convertToDrumkitXML drumkit
                        drumkitFName = dgPath </> unpack (dkName drumkit) <.> ".xml"
                    B.writeFile drumkitFName drumkitCont

                    -- create the midimap
                    let mm = MidiMap (Prelude.zip [35..] insts)
                        insts = getInstrumentNames drumkit
                        midimapCont = convertToMidiMapXML mm
                        midiMapFName = dgPath </> unpack (dkName drumkit) ++ "_MidiMap" <.> ".xml"
                    B.writeFile midiMapFName midimapCont



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
