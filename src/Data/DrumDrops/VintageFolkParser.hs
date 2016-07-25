{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.VintageFolkParser
    (
    getSampleFromFileName
    ,determineChannel
    )
where


import Control.Monad (void)

import Data.Text as T
import Data.Types

import Text.Parsec as P

import System.FilePath

import Data.DrumDrops.Types

import Debug.Trace
import Text.Printf


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
        "GR10" -> generalParserWithTom fname nChannels maker' 1
        "GR12" -> generalParserWithTom fname nChannels maker' 2
        "GR13" -> generalParserWithTom fname nChannels maker' 3
        _ -> generalParser fname nChannels maker'



generalParser :: Text -> Word -> String -> Parsec Text u Sample
generalParser fname nChannels maker' = do
    void $ char '_'
    inst <- instrument 0
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


generalParserWithTom :: Text -> Word -> String -> Int -> Parsec Text u Sample
generalParserWithTom fname nChannels maker' tomNr = do
    void $ char '_'
    inst <- instrument tomNr
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



instrument :: Int -> Parsec Text u Instrument
instrument tomNr = do
    (try (string "SNR") >> return Snare)
    <|> (try (string "SN") >> return Snare)
    <|> (try (string "KICK") >> return Kick)
    <|> (try (string "KK") >> return Kick)
    <|> (try (string "HAT") >> return HiHat)
    <|> (try (string "HH") >> return HiHat)
    <|> try (toms tomNr)
    <|> (try (string "CRSH") >> return Cymbal)
    <|> (try (string "CRS") >> return Cymbal)
    <|> (try (string "RIDE") >> return Ride)
    <|> (try (string "RD") >> return Ride)


toms :: Int -> Parsec Text u Instrument
toms tomNr = do
    t <- try racktom
        <|> try racktomN
        <|> try floorTom
    return (Tom t)
    where
        racktom = do
            void $ try (string "RKTM")
            return (RackTom tomNr)
        racktomN = do
            void $ try (string "RTM") <|> string "RT"
            n <- digit
            return (RackTom (read [n]))
        floorTom = do
            void $ try (string "FLTM") <|> string "FTM"
            return (Floor 1)


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



determineChannel :: Sample -> Channel -> Microphones
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Close)}) Mono =
    KickC
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Close)}) LeftA =
    KickL
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Close)}) RightA =
    KickR
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Sub)}) Mono =
    KickS
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Overhead)}) LeftA =
    OHL
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Overhead)}) RightA =
    OHR
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Room)}) LeftA =
    RoomL
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Room)}) RightA =
    RoomR
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Close)}) Mono =
    SnareTop
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Close)}) LeftA =
    SnareL
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Close)}) RightA =
    SnareR
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Overhead)}) LeftA =
    OHL
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Overhead)}) RightA =
    OHR
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Room)}) LeftA =
    RoomL
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Room)}) RightA =
    RoomR
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Close)}) Mono =
    HiHatC
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Close)}) LeftA =
    HiHatL
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Close)}) RightA =
    HiHatR
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Room)}) LeftA =
    RoomL
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Room)}) RightA =
    RoomR
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Overhead)}) LeftA =
    OHL
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Overhead)}) RightA =
    OHR
determineChannel (Sample {saInstrument = Tom (RackTom x), saInstrumentProperties = (InstS Close)}) Mono =
    TomC x
determineChannel (Sample {saInstrument = Tom (RackTom x), saInstrumentProperties = (InstS Close)}) LeftA =
    TomL x
determineChannel (Sample {saInstrument = Tom (RackTom x), saInstrumentProperties = (InstS Close)}) RightA =
    TomR x
determineChannel (Sample {saInstrument = Tom (RackTom _), saInstrumentProperties = (InstS Room)}) LeftA =
    RoomL
determineChannel (Sample {saInstrument = Tom (RackTom _), saInstrumentProperties = (InstS Room)}) RightA =
    RoomR
determineChannel (Sample {saInstrument = Tom (RackTom _), saInstrumentProperties = (InstS Overhead)}) LeftA =
    OHL
determineChannel (Sample {saInstrument = Tom (RackTom _), saInstrumentProperties = (InstS Overhead)}) RightA =
    OHR
determineChannel (Sample {saInstrument = Tom (Floor x), saInstrumentProperties = (InstS Close)}) Mono =
    FloorTomC x
determineChannel (Sample {saInstrument = Tom (Floor x), saInstrumentProperties = (InstS Close)}) LeftA =
    FloorTomL x
determineChannel (Sample {saInstrument = Tom (Floor x), saInstrumentProperties = (InstS Close)}) RightA =
    FloorTomR x
determineChannel (Sample {saInstrument = Tom (Floor _), saInstrumentProperties = (InstS Room)}) LeftA =
    RoomL
determineChannel (Sample {saInstrument = Tom (Floor _), saInstrumentProperties = (InstS Room)}) RightA =
    RoomR
determineChannel (Sample {saInstrument = Tom (Floor _), saInstrumentProperties = (InstS Overhead)}) LeftA =
    OHL
determineChannel (Sample {saInstrument = Tom (Floor _), saInstrumentProperties = (InstS Overhead)}) RightA =
    OHR
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS Overhead)}) LeftA =
    OHL
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS Overhead)}) RightA =
    OHR
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS Room)}) LeftA =
    RoomL
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS Room)}) RightA =
    RoomR
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Overhead)}) LeftA =
    OHL
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Overhead)}) RightA =
    OHR
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Room)}) LeftA =
    RoomL
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Room)}) RightA =
    RoomR
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Close)}) Mono =
    RideC
-- Multi Velocity Kits
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS FullMix)}) LeftA =
    KickL
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS FullMix)}) RightA =
    KickR
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS FullMix)}) LeftA =
    SnareL
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS FullMix)}) RightA =
    SnareR
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ FullMix)}) LeftA =
    HiHatL
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ FullMix)}) RightA =
    HiHatR
determineChannel (Sample {saInstrument = Tom (RackTom x), saInstrumentProperties = (InstS FullMix)}) LeftA =
    TomL x
determineChannel (Sample {saInstrument = Tom (RackTom x), saInstrumentProperties = (InstS FullMix)}) RightA =
    TomR x
determineChannel (Sample {saInstrument = Tom (Floor x), saInstrumentProperties = (InstS FullMix)}) LeftA =
    FloorTomL x
determineChannel (Sample {saInstrument = Tom (Floor x), saInstrumentProperties = (InstS FullMix)}) RightA =
    FloorTomR x
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS FullMix)}) LeftA =
    RideL
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS FullMix)}) RightA =
    RideR
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS FullMix)}) LeftA =
    OHL
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS FullMix)}) RightA =
    OHR

-- For the vintage folk kit

determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS KickClose)}) Mono =
    KickC
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS SnareClose)}) Mono =
    SnareTop
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Kit1)}) Mono =
    RoomL
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Kit2)}) Mono =
    RoomR
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Overhead)}) Mono =
    OHL
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS KickClose)}) Mono =
    KickC
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS SnareClose)}) Mono =
    SnareTop
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Kit1)}) Mono =
    RoomL
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Kit2)}) Mono =
    RoomR
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Overhead)}) Mono =
    OHL
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ KickClose)}) Mono =
    KickC
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ SnareClose)}) Mono =
    SnareTop
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Kit1)}) Mono =
    RoomL
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Kit2)}) Mono =
    RoomR
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Overhead)}) Mono =
    OHL
determineChannel (Sample {saInstrument = Tom _, saInstrumentProperties = (InstS KickClose)}) Mono =
    KickC
determineChannel (Sample {saInstrument = Tom _, saInstrumentProperties = (InstS SnareClose)}) Mono =
    SnareTop
determineChannel (Sample {saInstrument = Tom _, saInstrumentProperties = (InstS Kit1)}) Mono =
    RoomL
determineChannel (Sample {saInstrument = Tom _, saInstrumentProperties = (InstS Kit2)}) Mono =
    RoomR
determineChannel (Sample {saInstrument = Tom _, saInstrumentProperties = (InstS Overhead)}) Mono =
    OHL
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS KickClose)}) Mono =
    KickC
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS SnareClose)}) Mono =
    SnareTop
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS Kit1)}) Mono =
    RoomL
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS Kit2)}) Mono =
    RoomR
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS Overhead)}) Mono =
    OHL
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS KickClose)}) Mono =
    KickC
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS SnareClose)}) Mono =
    SnareTop
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Kit1)}) Mono =
    RoomL
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Kit2)}) Mono =
    RoomR
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Overhead)}) Mono =
    OHL
determineChannel (Sample {saInstrument = Tambourine, saInstrumentProperties = (InstS Close)}) Mono =
    TambourineC
determineChannel (Sample {saInstrument = Shaker, saInstrumentProperties = (InstS Close)}) Mono =
    ShakerC


determineChannel sample channel = trace (printf "%s %s" (show sample) (show channel)) Undefined
