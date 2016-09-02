{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.Import
    (
    importMidiMap
    ,importInstrumentFile
    ,importDrumkitFile
    )
where


import Data.Text (Text, pack, unpack, append)
import Data.Text.Read
import Data.Types
import Text.XML.Stream.Parse
import Data.XML.Types
--import Data.Maybe
import Data.Either
import System.FilePath
--import Data.Monoid ((<>))

import Control.Monad.Trans.Resource
import Data.Conduit

import Control.Exception
import Data.Typeable


data DKParseException =
    AudioFileParseError { dkpeMsg :: Text }
    | SampleFileParseError { dkpeMsg :: Text }
    | DrumkitParseError { dkpeMsg :: Text }
    deriving (Show, Typeable)

instance Exception DKParseException



parseMidiNote :: MonadThrow m => ConduitM Event o m (Maybe (Int, Text))
parseMidiNote = tagName "map" ((,) <$> (requireAttr "note") <*> (requireAttr "instr")) $ \(note, instr) ->
    return $ (read (unpack note), instr)


parseMidiMap :: MonadThrow m => ConduitM Event o m (Maybe MidiMap)
parseMidiMap = do
    mms <- tagNoAttr "midimap" $ many parseMidiNote
    case mms of
        Nothing -> return Nothing
        Just x -> return $ Just (MidiMap x)


importMidiMap :: FilePath -> IO (Maybe MidiMap)
importMidiMap path = do
    mm <- runResourceT $
        parseFile def path $$ parseMidiMap
    return mm




parseInstrument :: MonadThrow m => FilePath -> ConduitM Event o m (Maybe InstrumentFile)
parseInstrument fname = do
    inst <- tagName "instrument" ((,) <$> requireAttr "version" <*> requireAttr "name") $
        \(version, name) -> do
            smpls <- tagNoAttr "samples" $ many parseSamples
            return (version, name, smpls)
    case inst of
        Just (vers, nam, Just smpl) -> return $ Just (InstrumentFile vers nam (pack fname) Nothing smpl)
        _ -> return Nothing



parseSamples :: MonadThrow m => ConduitM Event o m (Maybe HitSample)
parseSamples =
    tagName "sample" ((,) <$> requireAttr "name" <*> requireAttr "power") $ \(name, power) -> do
        af <- many parseAudioFile
        let p = double power
        if isLeft p
            then throwM (SampleFileParseError ("Invalid Power specified for Sample: " `append` name))
            else do
                let Right (x, _) = p
                return (HitSample name x af)


parseAudioFile :: MonadThrow m => ConduitM Event o m (Maybe AudioFile)
parseAudioFile = do
    tagName "audiofile" attrs $ \af -> return af
    where
        attrs = do
            chan <- requireAttr "channel"
            file <- requireAttr "file"
            filechannel <- requireAttr "filechannel"
            let
                filechannel' = decimal filechannel

            if isLeft filechannel'
                then throwM (AudioFileParseError ("Invalid Filechannel for file: " `append` file))
                else do
                    let Right (x, _) = filechannel'
                    return $ AudioFile chan (unpack file) x Nothing


importInstrumentFile :: FilePath -> IO (Either Text InstrumentFile)
importInstrumentFile path = do
    catches worker [Handler handler, Handler handler2]
    where
        worker = do
            iF <- runResourceT $
                parseFile def path $$ parseInstrument (takeFileName path)
            return (maybe (Left "Could not parse file") Right iF)
        handler e = return (Left (dkpeMsg e))
        handler2 XmlException{..} = do
            let msg = (pack xmlErrorMessage) `append` "\n\nContext: " `append` (pack (show xmlBadInput))
            return (Left msg)
        handler2 e = return (Left (pack (show e)))






conduitDrumKitXML :: MonadThrow m => ConduitM Event o m (Maybe Drumkit)
conduitDrumKitXML = do
    tagName "drumkit" ((,) <$> requireAttr "name" <*> requireAttr "description" ) $ \(name, description) -> do
        chans <- channels
        insts <- instruments
        case (chans, insts) of
            (Just c, Just i) -> return $ Drumkit name description c i
            _ -> throwM (DrumkitParseError "Cannot parse drumkit")
    where
        channels = tagNoAttr "channels" (many ch)
        ch = tagName "channel" (requireAttr "name" ) return
        instruments = tagNoAttr "instruments" (many ins)
        ins = tagName "instrument" ((,,) <$> requireAttr "name" <*> attr "group" <*> requireAttr "file" ) $ \(name, group, file) -> do
            cm <- many channelmap
            return $ ChannelMap name group (unpack file) Nothing cm (cmCheckUndefined cm)
        channelmap = tagName "channelmap" ((,) <$> requireAttr "in" <*> requireAttr "out") return



importDrumkitFile :: FilePath -> IO (Either Text Drumkit)
importDrumkitFile path = do
    catches worker [Handler handler, Handler handler2]
    where
        worker = do
            iF <- runResourceT $
                parseFile def path $$ conduitDrumKitXML
            return (maybe (Left "Could not parse file") Right iF)
        handler e = return (Left (dkpeMsg e))
        handler2 XmlException{..} = do
            let msg = (pack xmlErrorMessage) `append` "\n\nContext: " `append` (pack (show xmlBadInput))
            return (Left msg)
        handler2 e = return (Left (pack (show e)))

