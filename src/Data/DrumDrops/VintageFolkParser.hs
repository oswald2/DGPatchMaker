{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.VintageFolkParser
    (
    getSampleFromFileName
    )
where


import Control.Monad (void)

import Data.Text as T
import Data.Types
--import Data.Maybe (isJust)
--import Data.Drumgizmo
--import Data.List (sort)

import Text.Parsec as P
--import Text.Parsec.Char

import System.FilePath

import Data.DrumDrops.Types



getSampleFromFileName :: FilePath -> Int -> Either ParseError Sample
getSampleFromFileName name nChannels =
    parse (sampleParser fname (fromIntegral nChannels)) fname' sname
    where
        fname' = takeFileName name
        fname = pack fname'
        sname = pack $ dropExtension fname'



sampleParser :: Text -> Word -> Parsec Text u Sample
sampleParser fname nChannels = do
    maker' <- many1 alphaNum
    case maker' of
        "SHKR" -> shakerParser fname nChannels
        "TAM1" -> tambourineParser fname nChannels
        "TAM" -> tambourineParser fname nChannels
        _ -> do
            void $ char '_'
            inst <- instrument
            void $ char '_'
            st <- case inst of
                HiHat -> hiHat
                _ -> instState
            void $ char '_'
            kitNumber
            void $ char '_'
            v <- velocity

            rr <- roundRobin

            let
                maker = pack maker'
                res =
                    Sample fname maker inst st v rr nChannels

            return res

shakerParser :: Text -> Word -> Parsec Text u Sample
shakerParser fname nChannels = do
    let inst = Shaker
    void $ manyTill anyChar (try kitNumber)
    void $ char '_'
    v <- velocity

    rr <- roundRobin
    let res = Sample fname "" inst (InstS Close) v rr nChannels
    return res

tambourineParser :: Text -> Word -> Parsec Text u Sample
tambourineParser fname nChannels = do
    let inst = Tambourine
    void $ manyTill anyChar (try kitNumber)
    void $ char '_'
    v <- velocity

    rr <- roundRobin
    let res = Sample fname "" inst (InstS Close) v rr nChannels
    return res



instrument :: Parsec Text u Instrument
instrument = do
    (try (string "SNR") >> return Snare)
    <|> (try (string "SN") >> return Snare)
    <|> (try (string "KICK") >> return Kick)
    <|> (try (string "KK") >> return Kick)
    <|> (try (string "HAT") >> return HiHat)
    <|> (try (string "HH") >> return HiHat)
    <|> try toms
    <|> (try (string "CRSH") >> return Cymbal)
    <|> (try (string "CRS") >> return Cymbal)
    <|> (try (string "RIDE") >> return Ride)
    <|> (try (string "RD") >> return Ride)


toms :: Parsec Text u Instrument
toms = do
    t <- try $ do
            void $ try (string "RTM") <|> string "RT"
            n <- digit
            return (RackTom (read [n]))
        <|> do
            void $ try (string "FLTM") <|> string "FTM"
            return (Floor 1)
    return (Tom t)


micType :: Parsec Text u MicType
micType = do
    genTry "CL" Close
    <|> genTry "OH" Overhead
    <|> genTry "FL" FullMix
    <|> genTry "K1" Kit1
    <|> genTry "K2" Kit2
    <|> genTry "KK" KickClose
    <|> genTry "SN" SnareClose



genTry :: String -> a -> Parsec Text u a
genTry what whatC = try (string what) >> return whatC


hiHatState :: Parsec Text u HiHatState
hiHatState = do
    genTry "BRC" HiHatBrushClosed
    <|> genTry "BRO" HiHatBrushOpen
    <|> genTry "HRO" HiHatHotRodsOpen
    <|> genTry "HRC" HiHatHotRodsClosed
    <|> genTry "C1EG" HiHatFullClosed
    <|> genTry "C1" HiHatFullClosed
    <|> genTry "C2EG" HiHatClosed
    <|> genTry "C2" HiHatClosed
    <|> genTry "C3EG" HiHatOpenQuarter
    <|> genTry "C3" HiHatOpenQuarter
    <|> genTry "O1EG" HiHatOpenHalf
    <|> genTry "O1" HiHatOpenHalf
    <|> genTry "O2EG" HiHatOpen3Quart
    <|> genTry "O2" HiHatOpen3Quart
    <|> genTry "O3EG" HiHatOpen
    <|> genTry "O3" HiHatOpen
    <|> genTry "EGC1" HiHatFullClosed
    <|> genTry "EGC2" HiHatClosed
    <|> genTry "EGC3" HiHatOpenQuarter
    <|> genTry "EGO1" HiHatOpenHalf
    <|> genTry "EGO2" HiHatOpen3Quart
    <|> genTry "EGO3" HiHatOpen
    <|> genTry "PC" HiHatPedalShut
    <|> genTry "PS" HiHatPedalShut
    <|> genTry "PO" HiHatPedalOpen


hiHat :: Parsec Text u InstState
hiHat = do
    st <- hiHatState
    void $ optional (char '_')
    mt <- micType
    return (HiHatS st mt)



instState :: Parsec Text u InstState
instState = do
    void $ manyTill anyChar (try (lookAhead micType))
    mt <- micType
    sb <- try (string "SB") <|> return ""
    case sb of
        "SB" -> return (InstS Sub)
        _ -> return (InstS mt)


kitNumber :: Parsec Text u ()
kitNumber = do
    void $ string "HT"
    void $ optional (char '_')
    void $ many1 digit
    return ()


velocity :: Parsec Text u Int
velocity = do
    void $ optional (char 'V')
    n <- many1 digit
    return (read n)


roundRobin :: Parsec Text u (Maybe Int)
roundRobin = do
    try $ do
            void $ string "_RR"
            n <- many1 digit
            return (Just (read n))
        <|>
            return Nothing



