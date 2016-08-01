{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.Import
    (
    importMidiMap
    )
where


import Data.Text (Text, unpack)
import Data.Text.Read
import Data.Types
import Text.XML.Stream.Parse
import Data.XML.Types
import Data.Maybe
import Data.Either

import Data.Monoid ((<>))

import Control.Monad.Trans.Resource
import Data.Conduit


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




--parseInstrument :: MonadThrow m => ConduitM Event o m (Maybe InstrumentFile)
--parseInstrument =
    --tagName "instrument" (requireAttr "version" <> requireAttr "name") $ many parseSamples

--parseSamples :: MonadThrow m => ConduitM Event o m (Maybe HitSample)
--parseSamples =
    --tagName "sample" (requireAttr "name" <> requireAttr "power") $ many parseAudioFile


parseAudioFile :: MonadThrow m => ConduitM Event o m (Maybe AudioFile)
parseAudioFile = do
    tagName "audiofile" attrs
    where
        attrs = do
            chan <- requireAttr "channel"
            file <- requireAttr "file"
            filechannel <- requireAttr "filechannel"
            let chan' = (fmap fst . listToMaybe . reads) (unpack chan)
                filechannel' = decimal filechannel

            if isNothing chan' || isLeft filechannel'
                then do
                    return Nothing
                else do
                    let Right (x, _) = filechannel'
                    return $ Just $ AudioFile (fromJust chan') (unpack file) x


--importInstrumentFile :: FilePath -> IO (Maybe InstrumentFile)
--importInstrumentFile path = do
    --iF <- runResourceT $
        --parseFile def path $$ parseAudioFile
    --return iF
