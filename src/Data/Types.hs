{-# LANGUAGE OverloadedStrings #-}
module Data.Types

where


import Data.Text
import Data.Set as S
import System.FilePath


data Drumkit = Drumkit {
    dkName :: !Text,
    dkDescription :: !Text,
    dkChannels :: [Text],
    dkInstruments :: [ChannelMap]
} deriving (Show)

data ChannelMap = ChannelMap {
    cmName :: !Text,
    cmGroup :: Maybe Text,
    cmFile :: !FilePath,
    cmMap :: [(Text, Text)]
} deriving (Show)

data MidiMap = MidiMap {
    mmNote :: [(Int, Text)]
} deriving (Show)

data Instrument =
    Kick
    | Snare
    | HiHat
    | Cymbal
    | Ride
    | Tom TomType
    deriving (Show, Eq)

data TomType =
    RackTom !Int
    | Floor !Int
    deriving (Show, Eq)

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


data HitSample = HitSample {
    hsName :: !Text,
    hsPower :: !Double,
    hsSamples :: [AudioFile]
} deriving Show


data AudioFile = AudioFile {
    afChannel:: !Text,
    afPath :: !FilePath,
    afFileChannel :: !Word
} deriving Show


generateDrumkit :: Text -> Text -> [InstrumentFile] -> Drumkit
generateDrumkit name description ifl = res
    where
        res = Drumkit name description channels undefined
        channels' = getAvailableChannels ifl
        channels = toList channels'


instrumentFileToChannelMap :: InstrumentFile -> ChannelMap
instrumentFileToChannelMap ifl =
    ChannelMap (ifName ifl) grp filePath undefined
    where
        filePath = "Instruments" </> unpack (ifName ifl) <.> "xml"
        grp | ifType ifl == HiHat = Just "hihat"
            | otherwise = Nothing


getAvailableChannelsIF :: InstrumentFile -> Set Text -> Set Text
getAvailableChannelsIF ifl set = Prelude.foldr acc1 set (ifSamples ifl)
    where
        acc1 hs s = Prelude.foldr acc s (hsSamples hs)
        acc af s1 = S.insert (afChannel af) s1


getAvailableChannels :: [InstrumentFile] -> Set Text
getAvailableChannels ifl = Prelude.foldr getAvailableChannelsIF S.empty ifl

