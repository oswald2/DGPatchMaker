{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.Types


where


import Data.Monoid

import Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder as TL
import Data.Text.Format

import Text.Parsec as P
import Text.Parsec.Char

import System.FilePath


showSample :: Sample -> Text
showSample (CloseSample{..}) =
    toStrict . toLazyText $ fromText "CloseSample "
        <> fromText (pack (show saInstrument))
        <> fromLazyText (format "{}" (Only saVelocity))
        <> fromLazyText (format "{}" (Only (maybe 0 id saRound)))
showSample (SubSample{..}) =
    toStrict . toLazyText $ fromText "SubSample "
        <> fromText (pack (show saInstrument))
        <> fromLazyText (format "{}" (Only saVelocity))
        <> fromLazyText (format "{}" (Only (maybe 0 id saRound)))
showSample (OverheadsSample{..}) =
    toStrict . toLazyText $ fromText "OverheadsSample "
        <> fromText (pack (show saInstrument))
        <> fromLazyText (format "{}" (Only saVelocity))
        <> fromLazyText (format "{}" (Only (maybe 0 id saRound)))
showSample (RoomSample{..}) =
    toStrict . toLazyText $ fromText "RoomSample "
        <> fromText (pack (show saInstrument))
        <> fromLazyText (format "{}" (Only saVelocity))
        <> fromLazyText (format "{}" (Only (maybe 0 id saRound)))


-- data type for the samples
data Sample =
    CloseSample {
        saFileName :: Text,
        saMaker :: Text,
        saInstrument :: Instrument,
        saVelocity :: Int,
        saRound :: Maybe Int
    }
    | SubSample {
        saFileName :: Text,
        saMaker :: Text,
        saInstrument :: Instrument,
        saVelocity :: Int,
        saRound :: Maybe Int
    }
    | OverheadsSample {
        saFileName :: Text,
        saMaker :: Text,
        saInstrument :: Instrument,
        saVelocity :: Int,
        saRound :: Maybe Int
    }
    | RoomSample {
        saFileName :: Text,
        saMaker :: Text,
        saInstrument :: Instrument,
        saVelocity :: Int,
        saRound :: Maybe Int
    }
    deriving Show


data Instrument =
    Kick
    | Snare
    | HiHat
    | Cymbal
    | Ride
    | Tom TomType
    deriving Show

data TomType =
    RackTom Int
    | Floor Int
    deriving Show

data MicType =
    Close
    | Sub
    | Overhead
    | Room
    deriving Show

data HiHatState =
    HiHatFullClosed
    | HiHatClosed
    | HiHatOpenQuarter
    | HiHatOpenHalf
    | HiHatOpen3Quart
    | HiHatOpen
    | HiHatPedalShut
    | HiHatPedalOpen
    deriving Show


data InstState =
    HiHatS {
        hState :: HiHatState,
        hMicType :: MicType
    }
    | InstS {
        hMicType :: MicType
    }
    deriving Show


getMic :: InstState -> MicType
getMic = hMicType



getSampleFromFileName :: FilePath -> Either ParseError Sample
getSampleFromFileName name =
    parse (sampleParser fname) "" sname
    where
        fname' = takeFileName name
        fname = pack fname'
        sname = pack $ dropExtension fname'


sampleParser :: Text -> Parsec Text u Sample
sampleParser fname = do
    maker' <- many1 upper
    char '_'
    inst <- instrument
    char '_'
    st <- case inst of
        HiHat -> hiHat
        _ -> instState
    char '_'
    kitNumber
    char '_'
    v <- velocity

    rr <- roundRobin

    let
        maker = pack maker'
        res =
            case getMic st of
                Close -> CloseSample fname maker inst v rr
                Sub -> SubSample fname maker inst v rr
                Overhead -> OverheadsSample fname maker inst v rr
                Room -> RoomSample fname maker inst v rr

    return res


instrument = do
    (try (string "SNR") >> return Snare)
    <|> (try (string "KICK") >> return Kick)
    <|> (try (string "HAT") >> return HiHat)
    <|> try toms
    <|> (try (string "CRSH") >> return Cymbal)
    <|> (try (string "RIDE") >> return Ride)


toms = do
    t <- try $ do
            char 'R'
            n <- digit
            return (RackTom (read [n]))
        <|> do
            char 'F'
            return (Floor 1)
    string "TM"
    return (Tom t)


micType = do
    genTry "CL" Close
    <|> genTry "OH" Overhead
    <|> genTry "RM" Room



genTry :: String -> a -> Parsec Text u a
genTry what whatC = try (string what) >> return whatC


hiHatState = do
    genTry "C1" HiHatFullClosed
    <|> genTry "C2" HiHatClosed
    <|> genTry "C3" HiHatOpenQuarter
    <|> genTry "O1" HiHatOpenHalf
    <|> genTry "O2" HiHatOpen3Quart
    <|> genTry "O3" HiHatOpen
    <|> genTry "FS" HiHatPedalShut
    <|> genTry "FO" HiHatPedalOpen


hiHat = do
    st <- hiHatState
    mt <- try $ do
            string "EG"
            micType
        <|> do
            micType
    return (HiHatS st mt)

instState = do
    P.count 2 upper
    mt <- micType
    sb <- try (string "SB") <|> return ""
    case sb of
        "" -> return (InstS mt)
        "SB" -> return (InstS Sub)
    return (InstS mt)


kitNumber = do
    string "HT_"
    many1 digit
    return ()


velocity = do
    char 'V'
    n <- many1 digit
    return (read n)


roundRobin = do
    try $ do
            string "_RR"
            n <- many1 digit
            return (Just (read n))
        <|>
            return Nothing



