{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.ModernFolkParser
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


separator :: Parsec Text u ()
separator = void (char '-' <|> char '_')



sampleParser :: Text -> Word -> Parsec Text u Sample
sampleParser fname nChannels = do
    kitnr <- many1 alphaNum
    case kitnr of
        "SHKR" -> shakerParser fname nChannels
        "TAM1" -> tambourineParser fname nChannels
        "TAM" -> tambourineParser fname nChannels
        _ -> do
            maker' <- case kitnr == "DD014" || kitnr == "DD104" of
                        True -> do
                            separator
                            many1 alphaNum
                        False -> do
                            return kitnr
            separator
            (inst, hhstate) <- instrument
            separator
            st <- case inst of
                HiHat -> hiHat hhstate
                _ -> instState
            separator
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
    separator
    v <- velocity

    rr <- roundRobin
    let res = Sample fname "" inst (InstS FullMix) v rr nChannels
    return res

tambourineParser :: Text -> Word -> Parsec Text u Sample
tambourineParser fname nChannels = do
    let inst = Tambourine
    void $ manyTill anyChar (try kitNumber)
    separator
    v <- velocity

    rr <- roundRobin
    let res = Sample fname "" inst (InstS FullMix) v rr nChannels
    return res


instrument :: Parsec Text u (Instrument, HiHatState)
instrument = do
    (try (string "Snare") >> return (Snare, HiHatUndefined))
    <|> (try (string "SNR") >> return (Snare, HiHatUndefined))
    <|> (try (string "Kick") >> return (Kick, HiHatUndefined))
    <|> (try (string "KICK") >> return (Kick, HiHatUndefined))
    <|> (try (string "ClosedHatsEdge") >> return (HiHat, HiHatClosed))
    <|> (try (string "ClosedHats") >> return (HiHat, HiHatClosed))
    <|> (try (string "OpenHatsEdge") >> return (HiHat, HiHatOpen))
    <|> (try (string "OpenHats") >> return (HiHat, HiHatOpen))
    <|> (try (string "FullClosedHats") >> return (HiHat, HiHatFullClosed))
    <|> (try (string "FullClosedEdge") >> return (HiHat, HiHatFullClosed))
    <|> (try (string "PedalHats") >> return (HiHat, HiHatPedalOpen))
    <|> try toms
    <|> (try (string "PlainCrash") >> return (Cymbal, HiHatUndefined))
    <|> (try (string "ThinCrash") >> return (Cymbal, HiHatUndefined))
    <|> (try (string "CRSH") >> return (Cymbal, HiHatUndefined))
    <|> (try (string "Ride") >> return (Ride, HiHatUndefined))
    <|> (try (string "RIDE") >> return (Ride, HiHatUndefined))


toms :: Parsec Text u (Instrument, HiHatState)
toms = do
    t <- do
        void $ string "Tom"
        n <- digit
        return (RackTom (read [n]))
    return ((Tom t), HiHatUndefined)


micType :: Parsec Text u MicType
micType = do
    genTry "CL" Close
    <|> genTry "OH" Overhead
    <|> genTry "RM1" Room1
    <|> genTry "RM2" Room2
    <|> genTry "FL" FullMix
    <|> genTry "BL" FullMix
    <|> genTry "BT" Bottom
    <|> genTry "TP" Top
    <|> genTry "K1" Kit1
    <|> genTry "K2" Kit2
    <|> genTry "KK" KickClose
    <|> genTry "SN" SnareClose



genTry :: String -> a -> Parsec Text u a
genTry what whatC = try (string what) >> return whatC





hiHat :: HiHatState -> Parsec Text u InstState
hiHat st'' = do
    m <- modifier
    separator
    mt <- micType
    let st' = case st'' of
                HiHatPedalOpen -> if m == "Closed" then HiHatPedalShut else HiHatPedalOpen
                _ -> HiHatPedalOpen
        st = case m of
                "SticksHalf" -> HiHatOpenHalf
                _ -> st'
    return (HiHatS st mt)


instState :: Parsec Text u InstState
instState = do
    void $  modifier
    separator
    mt <- micType
    return (InstS mt)


modifier :: Parsec Text u Text
modifier = do
    m <- do
        try (string "Brushes")
        <|> try (string "Hotrods")
        <|> try (string "SticksCentre")
        <|> try (string "StickCentre")
        <|> try (string "SticksEdge")
        <|> try (string "SticksRimshot")
        <|> try (string "SticksSidestick")
        <|> try (string "StcksHalfEdge")
        <|> try (string "SticksHalfEdge")
        <|> try (string "SticksHalf")
        <|> try (string "SticksQuarterEdge")
        <|> try (string "SticksQuarter")
        <|> try (string "SticksThreeQuartersEdge")
        <|> try (string "SticksThreeQuarters")
        <|> try (string "Sticks")
        <|> try (string "Open")
        <|> try (string "Closed")
        <|> try (string "HardBeater")
        <|> try (string "SoftBeater")
        <|> try (string "Centre")
        <|> try (string "Edge")
        <|> try (string "Bell")
    return (pack m)

velocity :: Parsec Text u Int
velocity = do
    void $ optional (char 'V')
    n <- many1 digit
    return (read n)


roundRobin :: Parsec Text u (Maybe Int)
roundRobin = do
    try $ do
            separator
            void $ string "RR"
            n <- many1 digit
            return (Just (read n))
        <|>
            return Nothing


kitNumber :: Parsec Text u ()
kitNumber = do
    void $ string "HT"
    separator
    void $ many1 digit
    return ()




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
    FullMixL
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS FullMix)}) RightA =
    FullMixR
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS FullMix)}) LeftA =
    FullMixL
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS FullMix)}) RightA =
    FullMixR
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ FullMix)}) LeftA =
    FullMixL
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ FullMix)}) RightA =
    FullMixR
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

determineChannel (Sample {saInstrument = Tambourine, saInstrumentProperties = (InstS FullMix)}) LeftA =
    FullMixL
determineChannel (Sample {saInstrument = Tambourine, saInstrumentProperties = (InstS FullMix)}) RightA =
    FullMixR
determineChannel (Sample {saInstrument = Shaker, saInstrumentProperties = (InstS FullMix)}) LeftA =
    FullMixL
determineChannel (Sample {saInstrument = Shaker, saInstrumentProperties = (InstS FullMix)}) RightA =
    FullMixR

--- for the modern folk kit

determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Room1)}) Mono =
    Room1Mono
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Room2)}) Mono =
    Room2Mono

determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Top)}) Mono =
    SnareTop
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Bottom)}) Mono =
    SnareBottom
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Room1)}) Mono =
    Room1Mono
determineChannel (Sample {saInstrument = Snare, saInstrumentProperties = (InstS Room2)}) Mono =
    Room2Mono
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Room1)}) Mono =
    Room1Mono
determineChannel (Sample {saInstrument = HiHat, saInstrumentProperties = (HiHatS _ Room2)}) Mono =
    Room2Mono

determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS Room1)}) Mono =
    Room1Mono
determineChannel (Sample {saInstrument = Cymbal, saInstrumentProperties = (InstS Room2)}) Mono =
    Room2Mono
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Room1)}) Mono =
    Room1Mono
determineChannel (Sample {saInstrument = Ride, saInstrumentProperties = (InstS Room2)}) Mono =
    Room2Mono

determineChannel (Sample {saInstrument = Tom (RackTom _), saInstrumentProperties = (InstS Room1)}) Mono =
    Room1Mono
determineChannel (Sample {saInstrument = Tom (RackTom _), saInstrumentProperties = (InstS Room2)}) Mono =
    Room2Mono
determineChannel (Sample {saInstrument = Tom (Floor _), saInstrumentProperties = (InstS Room1)}) Mono =
    Room1Mono
determineChannel (Sample {saInstrument = Tom (Floor _), saInstrumentProperties = (InstS Room2)}) Mono =
    Room2Mono


determineChannel sample channel = trace (printf "%s %s" (show sample) (show channel)) Undefined
