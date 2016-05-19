{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.HitSample
    (
    HitSample(..)
    ,hsPowerAsString
    ,Sample(..)
    ,saFileChannelAsText
    )
where


import Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Format
import Data.Word

import Data.Vector as V


data HitSample = HitSample {
    hsName :: !Text,
    hsPower :: !Double,
    hsSamples :: Vector Sample
    } deriving Show


data Sample = Sample {
    saName :: !Text,
    saPath :: !FilePath,
    saChannel :: !Text,
    saFileChannel :: !Word
} deriving Show

hsPowerAsString :: HitSample -> Text
hsPowerAsString hs = toStrict (format "{}" (Only (shortest (hsPower hs))))


saFileChannelAsText :: Sample -> Text
saFileChannelAsText hs = toStrict (format "{}" (Only (saFileChannel hs)))
