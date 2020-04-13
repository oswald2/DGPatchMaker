{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.Export
  ( convertToTabSep
  , writeMidiMapXML
  , writeInstrumentXML
  , writeDrumKitXML
  )
where


import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( pack )
import qualified Data.Text.Lazy                as L
import           Data.Text.Lazy.Builder        as L
import           Data.Text.Lazy.Builder.Int    as L
import           Data.Types

import           Text.XML.Stream.Render
import           Data.XML.Types
import           Data.Conduit                   ( ConduitT )
import qualified Data.Conduit                  as C
import qualified Data.Conduit.Combinators      as C





writeInstrumentXML :: InstrumentFile -> FilePath -> IO ()
writeInstrumentXML iF filename = do
  C.runConduitRes
    $    conduitInstrumentSource iF
    C..| renderBytes (def { rsPretty = True })
    C..| C.sinkFile filename


conduitInstrumentSource :: Monad m => InstrumentFile -> ConduitT () Event m ()
conduitInstrumentSource iF = do
  C.yield EventBeginDocument
  conduitInstrumentXML iF


conduitInstrumentXML :: Monad m => InstrumentFile -> ConduitT () Event m ()
conduitInstrumentXML InstrumentFile {..} = tag
  "instrument"
  (attr "version" ifVersion <> attr "name" ifName)
  (tag "samples" mempty (conduitSamples ifSamples))


conduitSamples :: Monad m => [HitSample] -> ConduitT () Event m ()
conduitSamples = foldr f mempty
 where
  f x b =
    tag "sample"
        (attr "name" (hsName x) <> attr "power" ((pack . show . hsPower) x))
        (conduitAudioFiles (hsSamples x))
      <> b



conduitAudioFiles :: Monad m => [AudioFile] -> ConduitT () Event m ()
conduitAudioFiles = foldr f mempty
 where
  f x b =
    tag
        "audiofile"
        (attr "channel" (afChannel x) <> attr "file" (pack (afPath x)) <> attr
          "filechannel"
          ((pack . show . afFileChannel) x)
        )
        mempty
      <> b


conduitMidiMap :: Monad m => MidiMap -> ConduitT () Event m ()
conduitMidiMap (MidiMap mp) = tag "midimap" mempty noteEntries
 where
  noteEntries = foldr f mempty mp
  f (note, instr) b =
    tag "map" (attr "note" (pack (show note)) <> attr "instr" instr) mempty
      <> b

conduitFullMidiMap :: Monad m => MidiMap -> ConduitT () Event m ()
conduitFullMidiMap mm = do
  C.yield EventBeginDocument
  conduitMidiMap mm


writeMidiMapXML :: MidiMap -> FilePath -> IO ()
writeMidiMapXML mp filename = do
  C.runConduitRes
    $    conduitFullMidiMap mp
    C..| renderBytes (def { rsPretty = True })
    C..| C.sinkFile filename



conduitDrumKitXML :: Monad m => Drumkit -> ConduitT () Event m ()
conduitDrumKitXML dr = tag
  "drumkit"
  (  attr "name"        (dkName dr)
  <> attr "description" (dkDescription dr)
  <> optionalAttr "samplerate" (dkSampleRate dr)
  )
  (metadata (dkMeta dr) <> channels <> instruments)
 where

  channels = tag "channels" mempty (foldr ch mempty (dkChannels dr))
  ch x b = tag "channel" (attr "name" x) mempty <> b
  instruments = tag "instruments" mempty (foldr ins mempty (dkInstruments dr))
  ins x b =
    tag "instrument"
        (attr "name" (cmName x) <> gr x <> attr "file" (pack (cmFile x)))
        (channelmap x)
      <> b
  gr x = case cmGroup x of
    Just g  -> attr "group" g
    Nothing -> mempty
  channelmap x = foldr chm mempty (cmMap x)
  chm (ChannelMapItem c1 c2 mn) b = if mn
    then
      tag "channelmap"
          (attr "in" c1 <> attr "out" c2 <> attr "main" "true")
          mempty
        <> b
    else tag "channelmap" (attr "in" c1 <> attr "out" c2) mempty <> b


metadata :: Monad m => Maybe MetaData -> ConduitT () Event m ()
metadata Nothing  = mempty
metadata (Just m) = tag
  "metadata"
  mempty
  (  version 
  <> title 
  <> logo 
  <> description 
  <> license 
  <> notes 
  <> email 
  <> website
  )
 where
  version     = maybe mempty (tag "version" mempty . content) (metaVersion m)
  title       = maybe mempty (tag "title" mempty . content) (metaTitle m)
  logo        = maybe mempty (\v -> tag "logo" (attr "src" v) mempty) (metaLogo m)
  description = maybe mempty (tag "description" mempty . content) (metaDescription m)
  license     = maybe mempty (tag "license" mempty . content) (metaLicense m)
  notes       = maybe mempty (tag "notes" mempty . content) (metaNotes m)
  email       = maybe mempty (tag "email" mempty . content) (metaEMail m)
  website     = maybe mempty (tag "website" mempty . content) (metaWebsite m)



conduitFullDrumKit :: Monad m => Drumkit -> ConduitT () Event m ()
conduitFullDrumKit dr = do
  C.yield EventBeginDocument
  conduitDrumKitXML dr


writeDrumKitXML :: Drumkit -> FilePath -> IO ()
writeDrumKitXML dr filename = do
  C.runConduitRes
    $    conduitFullDrumKit dr
    C..| renderBytes (def { rsPretty = True })
    C..| C.sinkFile filename


convertToTabSep :: MidiMap -> L.Text
convertToTabSep mm =
  let ls = mmNote mm
      f (note, inst) b =
          fromText inst
            <> fromText "\t"
            <> decimal note
            <> fromText "\t"
            <> fromText (midiToNote note)
            <> fromText "\n"
            <> b
      header = "Instrument\tMIDI\tNote\n"
  in  toLazyText $ header <> foldr f (fromText "") ls

