{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.Types
    (
    Sample(..),
    SampleGroup(..),
    VelocityGroup(..),
    Channel(..),
    getMic,
    )
where


--import Control.Monad (void)

import Data.Text as T
import Data.Types
import Data.Maybe (isJust)




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
    sgSampleRate :: !Int,
    sgGroups :: [VelocityGroup]
} deriving (Show)

data VelocityGroup = VelocityGroup {
    vgVelocity :: Double,
    vgRR :: Maybe Int,
    vgInstrument :: !Instrument,
    vgSamples :: [Sample]
} deriving Show


data Channel =
    Mono
    | LeftA
    | RightA
    | MonoSplit
    deriving (Show)





