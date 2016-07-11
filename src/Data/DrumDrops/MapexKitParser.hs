{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.MapexKitParser
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
    parse (sampleParser fname (fromIntegral nChannels)) "" sname
    where
        fname' = takeFileName name
        fname = pack fname'
        sname = pack $ dropExtension fname'



sampleParser :: Text -> Word -> Parsec Text u Sample
sampleParser fname nChannels = do
    maker' <- many1 alphaNum
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


instrument :: Parsec Text u Instrument
instrument = do
    (try (string "SNR") >> return Snare)
    <|> (try (string "SN") >> return Snare)
    <|> (try (string "KICK") >> return Kick)
    <|> (try (string "HAT") >> return HiHat)
    <|> try toms
    <|> (try (string "CRSH") >> return Cymbal)
    <|> (try (string "RIDE") >> return Ride)


toms :: Parsec Text u Instrument
toms = do
    t <- try $ do
            void $ char 'R'
            n <- digit
            return (RackTom (read [n]))
        <|> do
            void $ string "FL"
            return (Floor 1)
    void $ string "TM"
    return (Tom t)


micType :: Parsec Text u MicType
micType = do
    genTry "CL" Close
    <|> genTry "OH" Overhead
    <|> genTry "RM" Room
    <|> genTry "FL" FullMix
    <|> genTry "K1" Kit1
    <|> genTry "K2" Kit2
    <|> genTry "KK" KickClose
    <|> genTry "SN" SnareClose



genTry :: String -> a -> Parsec Text u a
genTry what whatC = try (string what) >> return whatC


hiHatState :: Parsec Text u HiHatState
hiHatState = do
    genTry "C1" HiHatFullClosed
    <|> genTry "C2" HiHatClosed
    <|> genTry "C3" HiHatOpenQuarter
    <|> genTry "O1" HiHatOpenHalf
    <|> genTry "O2" HiHatOpen3Quart
    <|> genTry "O3" HiHatOpen
    <|> genTry "FS" HiHatPedalShut
    <|> genTry "FO" HiHatPedalOpen


hiHat :: Parsec Text u InstState
hiHat = do
    st <- hiHatState
    mt <- try $ do
            void $ string "EG"
            micType
        <|> do
            micType
    return (HiHatS st mt)



instState :: Parsec Text u InstState
instState = do
    void $ P.count 2 upper
    void $ optional (char '_')
    --void $ manyTill anyChar (try (lookAhead micType))
    mt <- micType
    sb <- try (string "SB") <|> return ""
    case sb of
        "SB" -> return (InstS Sub)
        _ -> return (InstS mt)


kitNumber :: Parsec Text u ()
kitNumber = do
    void $ string "HT_"
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



