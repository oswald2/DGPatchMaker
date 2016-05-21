{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.Types
    (
    Sample(..),
    SampleGroup(..),
    VelocityGroup(..),
    getMic,
    velocityGroup,
    getSampleFromFileName,
    sampleParser,
    convertSampleGroup,
    getMaxVelocity
    )
where


import Control.Monad (void)

import Data.Text as T
import Data.Types
import Data.Maybe (isJust)

import Text.Parsec as P
--import Text.Parsec.Char

import System.FilePath




-- data type for the samples
data Sample =
    Sample {
        saFileName :: !Text,
        saMaker :: !Text,
        saInstrument :: !Instrument,
        saInstrumentProperties :: !InstState,
        saVelocity :: !Int,
        saRound :: Maybe Int,
        saType :: !AudioType
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
    sgPath :: FilePath,
    sgInstName :: Text,
    sgGroups :: [VelocityGroup]
} deriving (Show)

data VelocityGroup = VelocityGroup {
    vgVelocity :: Double,
    vgRR :: Maybe Int,
    vgSamples :: [Sample]
} deriving Show


kickClose :: Text
kickClose = "KickC"

kickSub :: Text
kickSub = "KickS"

overheadL :: Text
overheadL = "OHL"

overheadR :: Text
overheadR = "OHR"

roomL :: Text
roomL = "RoomL"

roomR :: Text
roomR = "RoomL"


data AudioType = Mono | Stereo deriving (Eq, Ord, Enum, Show)


convertSampleGroup :: SampleGroup -> InstrumentFile
convertSampleGroup sg = undefined
    where
        maxVel = getMaxVelocity sg
        convertSample :: Sample -> [AudioFile]
        convertSample x =
            AudioFile (determineChannel x) (determinePath (sgPath sg)) (determineFileChannel x)

        --convertVelocityGroup :: VelocityGroup -> HitSample
        --convertVelocityGroup =


determineChannel :: Sample -> Text
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Close)} = kickClose
determineChannel (Sample {saInstrument = Kick, saInstrumentProperties = (InstS Close)} = kickClose




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
    void $ char '_'
    inst <- instrument
    void $ char '_'
    st <- case inst of
        HiHat -> hiHat
        _ -> instState
    void $ char '_'
    kitNumber
    void $ char '_'
    v <- velocity

    rr <- roundRobin

    let
        maker = pack maker'
        res =
            Sample fname maker inst st v rr

    return res


instrument :: Parsec Text u Instrument
instrument = do
    (try (string "SNR") >> return Snare)
    <|> (try (string "KICK") >> return Kick)
    <|> (try (string "HAT") >> return HiHat)
    <|> try toms
    <|> (try (string "CRSH") >> return Cymbal)
    <|> (try (string "RIDE") >> return Ride)


toms :: Parsec Text u Instrument
toms = do
    t <- try $ do
            void $ char 'R'
            n <- digit
            return (RackTom (read [n]))
        <|> do
            void $ char 'F'
            return (Floor 1)
    void $ string "TM"
    return (Tom t)


micType :: Parsec Text u MicType
micType = do
    genTry "CL" Close
    <|> genTry "OH" Overhead
    <|> genTry "RM" Room



genTry :: String -> a -> Parsec Text u a
genTry what whatC = try (string what) >> return whatC


hiHatState :: Parsec Text u HiHatState
hiHatState = do
    genTry "C1" HiHatFullClosed
    <|> genTry "C2" HiHatClosed
    <|> genTry "C3" HiHatOpenQuarter
    <|> genTry "O1" HiHatOpenHalf
    <|> genTry "O2" HiHatOpen3Quart
    <|> genTry "O3" HiHatOpen
    <|> genTry "FS" HiHatPedalShut
    <|> genTry "FO" HiHatPedalOpen


hiHat :: Parsec Text u InstState
hiHat = do
    st <- hiHatState
    mt <- try $ do
            void $ string "EG"
            micType
        <|> do
            micType
    return (HiHatS st mt)


instState :: Parsec Text u InstState
instState = do
    void $ P.count 2 upper
    mt <- micType
    sb <- try (string "SB") <|> return ""
    case sb of
        "SB" -> return (InstS Sub)
        _ -> return (InstS mt)


kitNumber :: Parsec Text u ()
kitNumber = do
    void $ string "HT_"
    void $ many1 digit
    return ()


velocity :: Parsec Text u Int
velocity = do
    void $ char 'V'
    n <- many1 digit
    return (read n)


roundRobin :: Parsec Text u (Maybe Int)
roundRobin = do
    try $ do
            void $ string "_RR"
            n <- many1 digit
            return (Just (read n))
        <|>
            return Nothing



