module Main

where

import System.FilePath

import Data.DrumDrops.Types
import Data.DrumDrops.Utils

import Data.Text.IO as T
import Data.Either


str1 = "MPEX_KICK_EQCL_HT_005_V1_RR1.wav"
path = "/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Kontakt Pack Samples/Kontakt Pack Samples/Mapex Kick Drum/EQ Head"




main = do

    files <- getFiles path
    case files of
        Left err -> T.putStrLn err
        Right fs -> do
            let res = map getSampleFromFileName fs
            Prelude.putStrLn "\n\nRights:\n\n"
            print $ map showSample (rights res)
            Prelude.putStrLn "\n\nLefts::\n\n"
            print (lefts res)
