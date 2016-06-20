{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.Import
    (
    importMidiMap
    )
where


import Data.Text (Text, unpack)
import Data.Types
import Text.XML.Stream.Parse
import Data.XML.Types

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
