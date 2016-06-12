{-# LANGUAGE OverloadedStrings #-}
module Data.Types

where

import Control.Monad (void)
import Data.Text
import Data.Set as S
import System.FilePath
import Text.Parsec as P
import Data.List (sortOn)


data Drumkit = Drumkit {
    dkName :: !Text,
    dkDescription :: !Text,
    dkChannels :: [Microphones],
    dkInstruments :: [ChannelMap]
} deriving (Show)

data ChannelMap = ChannelMap {
    cmName :: !Text,
    cmGroup :: Maybe Text,
    cmFile :: !FilePath,
    cmType :: !Instrument,
    cmMap :: [(Text, Text)]
} deriving (Show)

data MidiMap = MidiMap {
    mmNote :: [(Int, Text)]
} deriving (Show)

data Instrument =
    Kick
    | Snare
    | HiHat
    | Tom TomType
    | Cymbal
    | Ride
    deriving (Show, Read, Eq)

instance Ord Instrument where
    compare x1 x2 = compare (toNumber x1) (toNumber x2)



toNumber :: Instrument -> Int
toNumber Kick = 1
toNumber Snare = 2
toNumber HiHat = 3
toNumber (Tom _) = 4
toNumber Cymbal = 5
toNumber Ride = 6



data TomType =
    RackTom !Int
    | Floor !Int
    deriving (Show, Read, Eq)

data MicType =
    Close
    | Sub
    | Overhead
    | Room
    deriving (Show, Enum, Ord, Eq)

data HiHatState =
    HiHatFullClosed
    | HiHatClosed
    | HiHatOpenQuarter
    | HiHatOpenHalf
    | HiHatOpen3Quart
    | HiHatOpen
    | HiHatPedalShut
    | HiHatPedalOpen
    deriving (Show, Enum, Ord, Eq)


data InstState =
    HiHatS {
        hState :: !HiHatState,
        hMicType :: !MicType
    }
    | InstS {
        hMicType :: !MicType
    }
    deriving (Show, Eq)


getMic :: InstState -> MicType
getMic = hMicType



data InstrumentFile = InstrumentFile {
    ifVersion :: !Text,
    ifName :: !Text,
    ifType :: !Instrument,
    ifSamples :: [HitSample]
} deriving Show

iflDefaultVersion :: Text
iflDefaultVersion = "2.0"


data HitSample = HitSample {
    hsName :: !Text,
    hsPower :: !Double,
    hsSamples :: [AudioFile]
} deriving Show


data AudioFile = AudioFile {
    afChannel:: !Microphones,
    afPath :: !FilePath,
    afFileChannel :: !Word
} deriving (Show, Eq)

instance Ord AudioFile where
    compare x1 x2 = compare (afChannel x1) (afChannel x2)


data Microphones =
    KickC
    | KickL
    | KickR
    | KickS
    | SnareTop
    | SnareBottom
    | SnareL
    | SnareR
    | HiHatC
    | HiHatL
    | HiHatR
    | TomC Int
    | TomL Int
    | TomR Int
    | FloorTomC Int
    | FloorTomL Int
    | FloorTomR Int
    | RideC
    | OHL
    | OHR
    | RoomL
    | RoomR
    | Undefined


instance Show Microphones where
    show KickC =        "KickC"
    show KickL =        "KickL"
    show KickR =        "KickR"
    show KickS =        "KickS"
    show SnareTop =     "SnareTop"
    show SnareBottom =  "SnareBottom"
    show SnareL =       "SnareL"
    show SnareR =       "SnareR"
    show HiHatC =       "HiHatC"
    show HiHatL =       "HiHatL"
    show HiHatR =       "HiHatR"
    show (TomC x) =     "TomC" ++ show x
    show (TomL x) =     "TomL" ++ show x
    show (TomR x) =     "TomR" ++ show x
    show (FloorTomC x) =     "FloorTomC" ++ show x
    show (FloorTomL x) =     "FloorTomL" ++ show x
    show (FloorTomR x) =     "FloorTomR" ++ show x
    show (RideC) =      "RideC"
    show OHL =          "OHL"
    show OHR =          "OHR"
    show RoomL =        "RoomL"
    show RoomR =        "RoomR"
    show Undefined =    "Undefined"

instance Ord Microphones where
    compare x1 x2 = compare (micToInt x1) (micToInt x2)

micToInt :: Microphones -> Int
micToInt KickC = 0
micToInt KickL = 1
micToInt KickR = 2
micToInt KickS = 3
micToInt SnareTop = 4
micToInt SnareBottom = 5
micToInt SnareL = 6
micToInt SnareR = 7
micToInt HiHatC = 8
micToInt HiHatL = 9
micToInt HiHatR = 10
micToInt (TomC x) = 10 + x
micToInt (TomL x) = 20 + x
micToInt (TomR x) = 30 + x
micToInt (FloorTomC x) = 40 + x
micToInt (FloorTomL x) = 50 + x
micToInt (FloorTomR x) = 60 + x
micToInt RideC = 70
micToInt OHL = 71
micToInt OHR = 72
micToInt RoomL = 73
micToInt RoomR = 74
micToInt Undefined = 100

instance Eq Microphones where
    x1 == x2 = (micToInt x1) == (micToInt x2)

micParser :: Parsec Text u Microphones
micParser = do
    (try (string "KickC"        ) >> return KickC       )
    <|> (try (string "KickL"        ) >> return KickL       )
    <|> (try (string "KickR"        ) >> return KickR       )
    <|> (try (string "KickS"        ) >> return KickS       )
    <|> (try (string "SnareTop"     ) >> return SnareTop    )
    <|> (try (string "SnareBottom"  ) >> return SnareBottom )
    <|> (try (string "SnareL"       ) >> return SnareL      )
    <|> (try (string "SnareR"       ) >> return SnareR      )
    <|> (try (string "HiHatC"       ) >> return HiHatC      )
    <|> (try (string "HiHatL"       ) >> return HiHatL      )
    <|> (try (string "HiHatR"       ) >> return HiHatR      )
    <|> do
        void $ try (string "TomC")
        n <- many1 digit
        return (TomC (read n))
    <|> do
        void $ try (string "TomL")
        n <- many1 digit
        return (TomL (read n))
    <|> do
        void $ try (string "TomR")
        n <- many1 digit
        return (TomR (read n))
    <|> do
        void $ try (string "FloorTomC")
        n <- many1 digit
        return (FloorTomC (read n))
    <|> do
        void $ try (string "FloorTomL")
        n <- many1 digit
        return (FloorTomL (read n))
    <|> do
        void $ try (string "FloorTomR")
        n <- many1 digit
        return (FloorTomR (read n))
    <|> (try (string "RideC"        ) >> return RideC       )
    <|> (try (string "OHL"          ) >> return OHL         )
    <|> (try (string "OHR"          ) >> return OHR         )
    <|> (try (string "RoomL"        ) >> return RoomL       )
    <|> (try (string "RoomR"        ) >> return RoomR       )
    <|> return Undefined


validateMic :: Text -> Either Text Microphones
validateMic txt =
    case parse micParser "" txt of
        Left err -> Left (pack (show err))
        Right mic -> Right mic


generateDrumkit :: Text -> Text -> [InstrumentFile] -> Drumkit
generateDrumkit name description ifl = res
    where
        res = Drumkit name description channels chanMap
        channels' = getAvailableChannels ifl
        channels = toAscList channels'

        chanMap = Prelude.map instrumentFileToChannelMap ifl


instrumentFileToChannelMap :: InstrumentFile -> ChannelMap
instrumentFileToChannelMap ifl =
    ChannelMap (ifName ifl) grp filePath (ifType ifl) chans
    where
        filePath = "Instruments" </> unpack (ifName ifl) <.> "xml"
        grp | ifType ifl == HiHat = Just "hihat"
            | otherwise = Nothing
        chans' = getAvailableChannelsIF ifl S.empty
        chans = Prelude.map (\x -> (x, x)) . Prelude.map (pack . show) $ toAscList chans'



getAvailableChannelsIF :: InstrumentFile -> Set Microphones -> Set Microphones
getAvailableChannelsIF ifl set = Prelude.foldr acc1 set (ifSamples ifl)
    where
        acc1 hs s = Prelude.foldr acc s (hsSamples hs)
        acc af s1 = S.insert (afChannel af) s1


getAvailableChannels :: [InstrumentFile] -> Set Microphones
getAvailableChannels ifl = Prelude.foldr getAvailableChannelsIF S.empty ifl



getInstrumentNames :: Drumkit -> [Text]
getInstrumentNames dk = Prelude.map cmName $ dkInstruments dk



getMidiMap :: Drumkit -> MidiMap
getMidiMap dk = MidiMap (sortOn fst (Prelude.map f (dkInstruments dk)))
    where
        f inst = (getMidiNoteFromInstrument (cmType inst), cmName inst)



getMidiNoteFromInstrument :: Instrument -> Int
getMidiNoteFromInstrument Kick = 35
getMidiNoteFromInstrument Snare = 38
getMidiNoteFromInstrument HiHat = 42
getMidiNoteFromInstrument (Tom (Floor _)) = 43
getMidiNoteFromInstrument (Tom (RackTom _)) = 45
getMidiNoteFromInstrument Cymbal = 55
getMidiNoteFromInstrument Ride = 51

