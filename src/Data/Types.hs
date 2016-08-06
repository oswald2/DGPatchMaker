{-# LANGUAGE OverloadedStrings #-}
module Data.Types

where

import Control.Monad (void)
import Data.Text
import Data.Set as S
import Data.List as L ((\\))
import System.FilePath
import Text.Parsec as P
import Data.List (sortOn)
import qualified Data.IntMap.Strict as M

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
    cmMap :: [(Text, Text)],
    cmContainsUndefined :: Bool
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
    | Shaker
    | Tambourine
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
toNumber Shaker = 7
toNumber Tambourine = 8



data TomType =
    RackTom !Int
    | Floor !Int
    deriving (Show, Read, Eq)

data MicType =
    Close
    | Sub
    | Overhead
    | Room
    | Room1
    | Room2
    | FullMix
    | Kit1
    | Kit2
    | KickClose
    | SnareClose
    | Top
    | Bottom
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
    | HiHatBrushOpen
    | HiHatBrushClosed
    | HiHatHotRodsOpen
    | HiHatHotRodsClosed
    | HiHatUndefined
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
    afFileChannel :: !Word,
    afPower :: Maybe Double
} deriving (Show, Read, Eq)

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
    | RideL
    | RideR
    | OHL
    | OHR
    | RoomL
    | RoomR
    | Room1Mono
    | Room2Mono
    | FullMixL
    | FullMixR
    | ShakerC
    | TambourineC
    | Undefined
    deriving (Show, Read)


showMic :: Microphones -> String
showMic KickC =        "KickC"
showMic KickL =        "KickL"
showMic KickR =        "KickR"
showMic KickS =        "KickS"
showMic SnareTop =     "SnareTop"
showMic SnareBottom =  "SnareBottom"
showMic SnareL =       "SnareL"
showMic SnareR =       "SnareR"
showMic HiHatC =       "HiHatC"
showMic HiHatL =       "HiHatL"
showMic HiHatR =       "HiHatR"
showMic (TomC x) =     "TomC" ++ show x
showMic (TomL x) =     "TomL" ++ show x
showMic (TomR x) =     "TomR" ++ show x
showMic (FloorTomC x) =     "FloorTomC" ++ show x
showMic (FloorTomL x) =     "FloorTomL" ++ show x
showMic (FloorTomR x) =     "FloorTomR" ++ show x
showMic (RideC) =      "RideC"
showMic (RideL) =      "RideL"
showMic (RideR) =      "RideR"
showMic OHL =          "OHL"
showMic OHR =          "OHR"
showMic RoomL =        "RoomL"
showMic RoomR =        "RoomR"
showMic Room1Mono =    "Room1Mono"
showMic Room2Mono =    "Room2Mono"
showMic FullMixL =     "FullMixL"
showMic FullMixR =     "FullMixR"
showMic ShakerC =      "ShakerC"
showMic TambourineC =  "TambourineC"
showMic Undefined =    "Undefined"




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
micToInt RideL = 71
micToInt RideR = 72
micToInt OHL = 73
micToInt OHR = 74
micToInt RoomL = 75
micToInt RoomR = 76
micToInt FullMixL = 77
micToInt FullMixR = 78
micToInt ShakerC = 79
micToInt TambourineC = 80
micToInt Room1Mono = 81
micToInt Room2Mono = 82
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
    <|> (try (string "RideL"        ) >> return RideL       )
    <|> (try (string "RideR"        ) >> return RideR       )
    <|> (try (string "OHL"          ) >> return OHL         )
    <|> (try (string "OHR"          ) >> return OHR         )
    <|> (try (string "RoomL"        ) >> return RoomL       )
    <|> (try (string "RoomR"        ) >> return RoomR       )
    <|> (try (string "Room1Mono"    ) >> return Room1Mono   )
    <|> (try (string "Room2Mono"    ) >> return Room2Mono   )
    <|> (try (string "FullMixL"     ) >> return FullMixL    )
    <|> (try (string "FullMixR"     ) >> return FullMixR    )
    <|> (try (string "ShakerC"      ) >> return ShakerC     )
    <|> (try (string "TambourineC"  ) >> return TambourineC )
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
    ChannelMap (ifName ifl) grp filePath (ifType ifl) chans (cmCheckUndefined chans)
    where
        filePath = "Instruments" </> unpack (ifName ifl) <.> "xml"
        grp | ifType ifl == HiHat = Just "hihat"
            | otherwise = Nothing
        chans' = getAvailableChannelsIF ifl S.empty
        chans = Prelude.map (\x -> (x, x)) . Prelude.map (pack . showMic) $ toAscList chans'



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
getMidiNoteFromInstrument Shaker = 48
getMidiNoteFromInstrument Tambourine = 32


midiNotes :: M.IntMap Text
midiNotes = M.fromList (Prelude.zip midi finalNotes)
    where
        midi = [0 .. 127]
        octaves = [ (-2)..8] :: [Int]
        notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
        f oct note = note `append` pack (show oct)
        g oct = Prelude.map (f oct) notes
        finalNotes = Prelude.concatMap g octaves


midiToNote :: Int -> Text
midiToNote x = maybe "--" id $ M.lookup x midiNotes


hsRemoveSamples :: HitSample -> [AudioFile] -> HitSample
hsRemoveSamples hs samples = hs { hsSamples = hsSamples hs L.\\ samples }


hsAddSamples :: HitSample -> [AudioFile] -> HitSample
hsAddSamples hs samples = hs { hsSamples = hsSamples hs ++ samples }

hsReplaceSamples :: HitSample -> [AudioFile] -> HitSample
hsReplaceSamples hs samples = hs { hsSamples = samples }


cmChangeChannel :: Text -> Text -> ChannelMap -> ChannelMap
cmChangeChannel oldName newName cm = cm {cmMap = chans }
    where
        chans = Prelude.map chg (cmMap cm)
        chg (inC, outC) | outC == oldName = (inC, newName)
                        | otherwise = (inC, outC)

cmCheckUndefined :: [(Text, Text)] -> Bool
cmCheckUndefined = Prelude.any (== True) . Prelude.map ((== (pack (show Undefined))) . snd)

cmUpdateIfUndefined :: ChannelMap -> ChannelMap
cmUpdateIfUndefined cm = newCm
    where
        newCm = cm {cmContainsUndefined = val }
        val = cmCheckUndefined (cmMap cm)
