{-# LANGUAGE OverloadedStrings #-}
module Data.Types

where


import Data.Text
import Data.Set as S
import System.FilePath


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
    deriving (Show, Eq)

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
    afChannel:: !Microphones,
    afPath :: !FilePath,
    afFileChannel :: !Word
} deriving Show

data Microphones =
    KickC
    | KickL
    | KickR
    | KickS
    | OHL
    | OHR
    | RoomL
    | RoomR
    | Undefined
    deriving (Show, Eq, Enum, Ord)





generateDrumkit :: Text -> Text -> [InstrumentFile] -> Drumkit
generateDrumkit name description ifl = res
    where
        res = Drumkit name description channels chanMap
        channels' = getAvailableChannels ifl
        channels = toAscList channels'

        chanMap = Prelude.map instrumentFileToChannelMap ifl


instrumentFileToChannelMap :: InstrumentFile -> ChannelMap
instrumentFileToChannelMap ifl =
    ChannelMap (ifName ifl) grp filePath chans
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
