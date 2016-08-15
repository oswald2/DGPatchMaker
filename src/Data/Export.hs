{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.Export
    (
    convertToTabSep
    ,writeMidiMapXML
    ,writeInstrumentXML
    ,writeDrumKitXML
    )
where


import Data.Monoid ((<>))
import Data.Text (pack)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder as L
import Data.Text.Lazy.Builder.Int as L
import Data.Types
--import qualified Data.ByteString.Lazy as B

import Text.XML.Stream.Render
import Data.XML.Types
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Control.Monad.Trans.Resource





writeInstrumentXML :: InstrumentFile -> FilePath -> IO ()
writeInstrumentXML iF filename = do
    runResourceT $ conduitInstrumentSource iF C.=$= renderText (def {rsPretty = True}) C.$$ C.sinkFile filename


conduitInstrumentSource :: Monad m => InstrumentFile -> C.Source m Event
conduitInstrumentSource iF = do
    C.yield EventBeginDocument
    conduitInstrumentXML iF


conduitInstrumentXML :: Monad m => InstrumentFile -> C.Source m Event
conduitInstrumentXML (InstrumentFile{..}) =
    tag "instrument" (attr "version" ifVersion <> attr "name" ifName) (tag "samples" mempty (conduitSamples ifSamples))


conduitSamples:: Monad m => [HitSample] -> C.Source m Event
conduitSamples ss =
    foldr f mempty ss
    where
        f x b = tag "sample" (attr "name" (hsName x) <> attr "power" ((pack.show.hsPower) x)) (conduitAudioFiles (hsSamples x)) <> b



conduitAudioFiles:: Monad m => [AudioFile] -> C.Source m Event
conduitAudioFiles afs =
    foldr f mempty afs
    where
        f x b = tag "audiofile" (attr "channel" (pack (show (afChannel x)))
                                <> attr "file" (pack (afPath x))
                                <> attr "filechannel" ((pack.show.afFileChannel) x)) mempty <> b


conduitMidiMap :: Monad m => MidiMap -> C.Source m Event
conduitMidiMap (MidiMap mp) =
    tag "midimap" mempty noteEntries
    where
        noteEntries = foldr f mempty mp
        f (note, instr) b = (tag "map" (attr "note" (pack (show note)) <> attr "instr" instr) mempty) <> b

conduitFullMidiMap :: Monad m => MidiMap -> C.Source m Event
conduitFullMidiMap mm = do
    C.yield EventBeginDocument
    conduitMidiMap mm


writeMidiMapXML :: MidiMap -> FilePath -> IO ()
writeMidiMapXML mp filename = do
    runResourceT $ conduitFullMidiMap mp C.=$= renderText (def {rsPretty = True}) C.$$ C.sinkFile filename



conduitDrumKitXML :: Monad m => Drumkit -> C.Source m Event
conduitDrumKitXML dr =
    tag "drumkit" (attr "name" (dkName dr) <> attr "description" (dkDescription dr)) (channels <> instruments)
    where
        channels = tag "channels" mempty (foldr ch mempty (dkChannels dr))
        ch x b = tag "channel" (attr "name" x) mempty <> b
        instruments = tag "instruments" mempty ((foldr ins mempty (dkInstruments dr)))
        ins x b = tag "instrument" (attr "name" (cmName x) <> gr x <> attr "file" (pack (cmFile x))) (channelmap x) <> b
        gr x = case cmGroup x of
                    Just g -> attr "group" g
                    Nothing -> mempty
        channelmap x = foldr chm mempty (cmMap x)
        chm (c1, c2) b = tag "channelmap" (attr "in" c1 <> attr "out" c2) mempty <> b

conduitFullDrumKit :: Monad m => Drumkit -> C.Source m Event
conduitFullDrumKit dr = do
    C.yield EventBeginDocument
    conduitDrumKitXML dr


writeDrumKitXML :: Drumkit -> FilePath -> IO ()
writeDrumKitXML dr filename = do
    runResourceT $ conduitFullDrumKit dr C.=$= renderText (def {rsPretty = True}) C.$$ C.sinkFile filename


convertToTabSep :: MidiMap -> L.Text
convertToTabSep mm =
    let ls = mmNote mm
        f (note, inst) b = fromText inst <> fromText "\t" <> decimal note <> fromText "\t" <> fromText (midiToNote note) <> fromText "\n" <> b
        header = "Instrument\tMIDI\tNote\n"
    in
    toLazyText $ header <> foldr f (fromText "") ls

