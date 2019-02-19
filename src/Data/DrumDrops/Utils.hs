{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}
module Data.DrumDrops.Utils where


import           Prelude                       as P

import           System.Directory
import           System.FilePath
import           Data.List                     as L
                                                ( sort
                                                , groupBy
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , append
                                                )

import           Data.Either
import           Data.Char                      ( isSpace )
import           Data.DrumDrops.Types
import qualified Data.DrumDrops.MapexKitParser as MP
import qualified Data.DrumDrops.VintageFolkParser
                                               as VFP
import qualified Data.DrumDrops.ModernFolkParser
                                               as MFP

import           Data.Types
import           Data.Drumgizmo

import           Sound.File.Sndfile             ( getFileInfo
                                                , Info(..)
                                                )

import           Text.Parsec                    ( ParseError )



data ParserType =
    MapexParser
    | VintageFolkParser
    | ModernFolkParser
    deriving (Enum, Eq, Ord, Show, Read)




importInstrument
    :: ParserType
    -> FilePath
    -> FilePath
    -> FilePath
    -> IO (Either Text (InstrumentFile, Int))
importInstrument parserType basepath samplesPath path = do

    putStrLn $ "Importing Instrument from: " ++ path

    w <- getSamples parserType samplesPath path
    case w of
        Left  err      -> return (Left err)
        Right wavFiles -> return
            (Right
                ( convertSampleGroup parserType basepath wavFiles
                , sgSampleRate wavFiles
                )
            )



getFiles :: FilePath -> IO (Either Text [FilePath])
getFiles path = do
    is <- doesDirectoryExist path
    if is
        then do
            cont' <- getDirectoryContents path
            let cont = P.filter (\x -> not (elem x [".", ".."])) cont'
            return (Right cont)
        else do
            return (Left (pack path `append` " is not a directory"))


getVelocityGroups :: [Sample] -> [VelocityGroup]
getVelocityGroups ss =
    let gs = (L.groupBy velocityGroup . sort) ss
        crV x =
            let first = head x
            in  VelocityGroup (fromIntegral (saVelocity first))
                              (saRound first)
                              (saInstrument first)
                              x
    in  map crV gs

getSamples :: ParserType -> FilePath -> FilePath -> IO (Either Text SampleGroup)
getSamples parserType samplesDir path = do
    fs <- getFiles path
    case fs of
        Left  err  -> return (Left err)
        Right cont -> do
            -- get audio information out of the wav files
            let
                proc c = do
                    info <- getFileInfo (path </> c)
                    return
                        ( getSampleFromFileName parserType c (channels info)
                        , (samplerate info)
                        )

            res <- mapM proc $ filter
                (\x -> takeExtension x == ".wav" || takeExtension x == ".WAV")
                cont

            let srate  = if null res then 44100 else (snd . head) res
                smpls  = map fst res
                spls   = rights smpls
                errors = lefts smpls

            if P.null errors
                then do
                    let gr = SampleGroup path
                                         (pathToInstrument samplesDir path)
                                         inst
                                         srate
                                         vgs
                        inst = vgInstrument (head vgs)
                        vgs  = getVelocityGroups spls
                    return (Right gr)
                else do
                    let err = "Failed parsing: " `append` pack (show errors)
                    return (Left err)


pathToInstrument :: FilePath -> FilePath -> Text
pathToInstrument sampleDir path' =
    let path = makeRelative sampleDir path'
        fs   = splitDirectories path
        ps   = P.concat fs
        inst = P.filter (not . isSpace) ps
    in  pack inst



getSampleFromFileName
    :: ParserType -> FilePath -> Int -> Either ParseError Sample
getSampleFromFileName MapexParser       = MP.getSampleFromFileName
getSampleFromFileName VintageFolkParser = VFP.getSampleFromFileName
getSampleFromFileName ModernFolkParser  = MFP.getSampleFromFileName



determineChannel :: ParserType -> Sample -> Channel -> Text
determineChannel parserType sample channel =
    pack . showMic $ case parserType of
        MapexParser       -> MP.determineChannel sample channel
        VintageFolkParser -> VFP.determineChannel sample channel
        ModernFolkParser  -> MFP.determineChannel sample channel


convertSampleGroup :: ParserType -> FilePath -> SampleGroup -> InstrumentFile
convertSampleGroup parserType basepath sg = InstrumentFile
    dgDefaultVersion
    nm
    fname
    (Just (sgInstrument sg))
    groups
  where
    nm = sgInstName sg
    vname :: Int -> Text
    vname i = nm `append` pack (show i)
    groups = P.zipWith
        (\vg i ->
            convertVelocityGroup parserType (vname i) (sgPath sg) basepath sg vg
        )
        (sgGroups sg)
        [1 ..]
    fname = nm `append` ".xml"


convertVelocityGroup
    :: ParserType -> Text -> FilePath -> FilePath -> SampleGroup -> VelocityGroup -> HitSample
convertVelocityGroup parserType name path basepath sampleGroup vg = HitSample
    name
    (vgVelocity vg)
    files
  where
    files =
        sort
            (P.concatMap (convertSample parserType basepath path sampleGroup) (vgSamples vg)
            )



convertSample
    :: ParserType
    -> FilePath
    -> FilePath
    -> SampleGroup
    -> Sample
    -> [AudioFile]
convertSample parserType basepath path sampleGroup x = case saChannels x of
    1 -> case saInstrumentProperties x of
        InstS FullMix ->
            [ AudioFile (determineChannel parserType x LeftA)
                        (determinePath basepath path (saFileName x))
                        1
                        Nothing
                        (Just (sgSampleRate sampleGroup))
            , AudioFile (determineChannel parserType x RightA)
                        (determinePath basepath path (saFileName x))
                        1
                        Nothing
                        (Just (sgSampleRate sampleGroup))
            ]
        _ ->
            [ AudioFile (determineChannel parserType x Mono)
                        (determinePath basepath path (saFileName x))
                        1
                        Nothing
                        (Just (sgSampleRate sampleGroup))
            ]
    2 ->
        [ AudioFile (determineChannel parserType x LeftA)
                    (determinePath basepath path (saFileName x))
                    1
                    Nothing
                    (Just (sgSampleRate sampleGroup))
        , AudioFile (determineChannel parserType x RightA)
                    (determinePath basepath path (saFileName x))
                    2
                    Nothing
                    (Just (sgSampleRate sampleGroup))
        ]
    _ -> []



determinePath :: FilePath -> FilePath -> Text -> FilePath
determinePath basepath path filename =
    "../../" </> makeRelative basepath path </> unpack filename




getMaxVelocity :: SampleGroup -> Double
getMaxVelocity (SampleGroup {..}) = P.maximum (P.map vgVelocity sgGroups)




velocityGroup :: Sample -> Sample -> Bool
velocityGroup x1 x2 =
    let v = saVelocity x1 == saVelocity x2
        rr (Just rr1) (Just rr2) = rr1 == rr2
        rr _          _          = True
        res = v && rr (saRound x1) (saRound x2)
    in  res
