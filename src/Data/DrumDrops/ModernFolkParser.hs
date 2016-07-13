{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.DrumDrops.ModernFolkParser
    (
    getSampleFromFileName
    )
where


import Control.Monad (void)

import Data.Text as T
import Data.Types
--import Data.Maybe (isJust)
--import Data.Drumgizmo
--import Data.List (sort)

import Text.Parsec as P
--import Text.Parsec.Char

import System.FilePath

import Data.DrumDrops.Types



getSampleFromFileName :: FilePath -> Int -> Either ParseError Sample
getSampleFromFileName name nChannels =
    parse (sampleParser fname (fromIntegral nChannels)) fname' sname
    where
        fname' = takeFileName name
        fname = pack fname'
        sname = pack $ dropExtension fname'


separator :: Parsec Text u ()
separator = void (char '-' <|> char '_')



sampleParser :: Text -> Word -> Parsec Text u Sample
sampleParser fname nChannels = do
    kitnr <- many1 alphaNum
    case kitnr of
        "SHKR" -> shakerParser fname nChannels
        "TAM1" -> tambourineParser fname nChannels
        "TAM" -> tambourineParser fname nChannels
        _ -> do
            maker' <- case kitnr == "DD014" || kitnr == "DD104" of
                        True -> do
                            separator
                            many1 alphaNum
                        False -> do
                            return kitnr
            separator
            (inst, hhstate) <- instrument
            separator
            st <- case inst of
                HiHat -> hiHat hhstate
                _ -> instState
            separator
            v <- velocity

            rr <- roundRobin

            let
                maker = pack maker'
                res =
                    Sample fname maker inst st v rr nChannels

            return res




shakerParser :: Text -> Word -> Parsec Text u Sample
shakerParser fname nChannels = do
    let inst = Shaker
    void $ manyTill anyChar (try kitNumber)
    separator
    v <- velocity

    rr <- roundRobin
    let res = Sample fname "" inst (InstS Close) v rr nChannels
    return res

tambourineParser :: Text -> Word -> Parsec Text u Sample
tambourineParser fname nChannels = do
    let inst = Tambourine
    void $ manyTill anyChar (try kitNumber)
    separator
    v <- velocity

    rr <- roundRobin
    let res = Sample fname "" inst (InstS Close) v rr nChannels
    return res


instrument :: Parsec Text u (Instrument, HiHatState)
instrument = do
    (try (string "Snare") >> return (Snare, HiHatUndefined))
    <|> (try (string "SNR") >> return (Snare, HiHatUndefined))
    <|> (try (string "Kick") >> return (Kick, HiHatUndefined))
    <|> (try (string "KICK") >> return (Kick, HiHatUndefined))
    <|> (try (string "ClosedHatsEdge") >> return (HiHat, HiHatClosed))
    <|> (try (string "ClosedHats") >> return (HiHat, HiHatClosed))
    <|> (try (string "OpenHatsEdge") >> return (HiHat, HiHatOpen))
    <|> (try (string "OpenHats") >> return (HiHat, HiHatOpen))
    <|> (try (string "FullClosedHats") >> return (HiHat, HiHatFullClosed))
    <|> (try (string "FullClosedEdge") >> return (HiHat, HiHatFullClosed))
    <|> (try (string "PedalHats") >> return (HiHat, HiHatPedalOpen))
    <|> try toms
    <|> (try (string "PlainCrash") >> return (Cymbal, HiHatUndefined))
    <|> (try (string "ThinCrash") >> return (Cymbal, HiHatUndefined))
    <|> (try (string "CRSH") >> return (Cymbal, HiHatUndefined))
    <|> (try (string "Ride") >> return (Ride, HiHatUndefined))
    <|> (try (string "RIDE") >> return (Ride, HiHatUndefined))


toms :: Parsec Text u (Instrument, HiHatState)
toms = do
    t <- do
        void $ string "Tom"
        n <- digit
        return (RackTom (read [n]))
    return ((Tom t), HiHatUndefined)


micType :: Parsec Text u MicType
micType = do
    genTry "CL" Close
    <|> genTry "OH" Overhead
    <|> genTry "RM1" Room1
    <|> genTry "RM2" Room2
    <|> genTry "FL" FullMix
    <|> genTry "BL" FullMix
    <|> genTry "BT" Bottom
    <|> genTry "TP" Top
    <|> genTry "K1" Kit1
    <|> genTry "K2" Kit2
    <|> genTry "KK" KickClose
    <|> genTry "SN" SnareClose



genTry :: String -> a -> Parsec Text u a
genTry what whatC = try (string what) >> return whatC





hiHat :: HiHatState -> Parsec Text u InstState
hiHat st'' = do
    m <- modifier
    separator
    mt <- micType
    let st' = case st'' of
                HiHatPedalOpen -> if m == "Closed" then HiHatPedalShut else HiHatPedalOpen
                _ -> HiHatPedalOpen
        st = case m of
                "SticksHalf" -> HiHatOpenHalf
                _ -> st'
    return (HiHatS st mt)


--instState :: Parsec Text u InstState
--instState = do
    --void $ P.count 2 upper
    --void $ optional (char '_')
    ----void $ manyTill anyChar (try (lookAhead micType))
    --mt <- micType
    --sb <- try (string "SB") <|> return ""
    --case sb of
        --"SB" -> return (InstS Sub)
        --_ -> return (InstS mt)

instState :: Parsec Text u InstState
instState = do
    void $  modifier
    separator
    mt <- micType
    return (InstS mt)


modifier :: Parsec Text u Text
modifier = do
    m <- do
        try (string "Brushes")
        <|> try (string "Hotrods")
        <|> try (string "SticksCentre")
        <|> try (string "StickCentre")
        <|> try (string "SticksEdge")
        <|> try (string "SticksRimshot")
        <|> try (string "SticksSidestick")
        <|> try (string "StcksHalfEdge")
        <|> try (string "SticksHalfEdge")
        <|> try (string "SticksHalf")
        <|> try (string "SticksQuarterEdge")
        <|> try (string "SticksQuarter")
        <|> try (string "SticksThreeQuartersEdge")
        <|> try (string "SticksThreeQuarters")
        <|> try (string "Sticks")
        <|> try (string "Open")
        <|> try (string "Closed")
        <|> try (string "HardBeater")
        <|> try (string "SoftBeater")
        <|> try (string "Centre")
        <|> try (string "Edge")
        <|> try (string "Bell")
    return (pack m)

velocity :: Parsec Text u Int
velocity = do
    void $ optional (char 'V')
    n <- many1 digit
    return (read n)


roundRobin :: Parsec Text u (Maybe Int)
roundRobin = do
    try $ do
            separator
            void $ string "RR"
            n <- many1 digit
            return (Just (read n))
        <|>
            return Nothing


kitNumber :: Parsec Text u ()
kitNumber = do
    void $ string "HT"
    separator
    void $ many1 digit
    return ()

