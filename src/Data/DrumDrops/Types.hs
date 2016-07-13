{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.Types
    (
    Sample(..),
    SampleGroup(..),
    VelocityGroup(..),
    getMic,
    velocityGroup,
    convertSampleGroup,
    getMaxVelocity
    )
where


--import Control.Monad (void)

import Data.Text as T
import Data.Types
import Data.Maybe (isJust)
import Data.Drumgizmo
import Data.List (sort)

import System.FilePath

import Debug.Trace
import Text.Printf


-- data type for the samples
data Sample =
    Sample {
        saFileName :: !Text,
        saMaker :: !Text,
        saInstrument :: !Instrument,
        saInstrumentProperties :: !InstState,
        saVelocity :: !Int,
        saRound :: Maybe Int,
        saChannels :: !Word
    }
    deriving (Show, Eq)

instance Ord Sample where
    compare (Sample {saVelocity = v1, saRound = rr1}) (Sample {saVelocity = v2, saRound = rr2}) =
        if v1 < v2
            then LT
            else if v1 == v2
                then
                    if isJust rr1 && isJust rr2
                        then compare rr1 rr2
                        else EQ
                else GT


data SampleGroup = SampleGroup {
    sgPath :: !FilePath,
    sgInstName :: !Text,
    sgInstrument :: !Instrument,
    sgGroups :: [VelocityGroup]
} deriving (Show)

data VelocityGroup = VelocityGroup {
    vgVelocity :: Double,
    vgRR :: Maybe Int,
    vgInstrument :: !Instrument,
    vgSamples :: [Sample]
} deriving Show




convertSampleGroup :: FilePath -> SampleGroup -> InstrumentFile
convertSampleGroup basepath sg =
    InstrumentFile dgDefaultVersion nm (sgInstrument sg) groups
    where
        nm = sgInstName sg
        vname :: Int -> Text
        vname i = nm `append` pack (show i)
        groups = Prelude.zipWith (\vg i -> convertVelocityGroup (vname i) (sgPath sg) basepath vg) (sgGroups sg) [1..]

data Channel =
    Mono
    | LeftA | RightA
    deriving (Show)



convertVelocityGroup :: Text -> FilePath -> FilePath -> VelocityGroup -> HitSample
convertVelocityGroup name path basepath vg =
    HitSample name (vgVelocity vg) files
    where
        files = sort (Prelude.concatMap (convertSample basepath path) (vgSamples vg))



convertSample :: FilePath -> FilePath -> Sample -> [AudioFile]
convertSample basepath path x =
    case saChannels x of
        1 -> [AudioFile (determineChannel x Mono) (determinePath basepath path (saFileName x)) 1]
        2 -> [AudioFile (determineChannel x LeftA) (determinePath basepath path (saFileName x)) 1,
              AudioFile (determineChannel x RightA) (determinePath basepath path (saFileName x)) 2]
        _ -> []


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


determinePath :: FilePath -> FilePath -> Text -> FilePath
determinePath basepath path filename = "../../" </> makeRelative basepath path </> unpack filename




getMaxVelocity :: SampleGroup -> Double
getMaxVelocity (SampleGroup{..}) = Prelude.maximum (Prelude.map vgVelocity sgGroups)




velocityGroup :: Sample -> Sample -> Bool
velocityGroup x1 x2 =
    let v = saVelocity x1 == saVelocity x2
        rr (Just rr1) (Just rr2) = rr1 == rr2
        rr _ _ = True
        res = v && rr (saRound x1) (saRound x2)
    in
    res

