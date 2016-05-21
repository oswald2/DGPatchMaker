module Data.Types

where


import Data.Text


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
