{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.MapexKitParser
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


determineChannel sample channel = trace (printf "%s %s" (show sample) (show channel)) Undefined

