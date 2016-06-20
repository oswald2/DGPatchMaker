{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.Export
    (
    convertToInstrumentXML,
    convertToMidiMapXML,
    convertToDrumkitXML,
    convertToTabSep
    )
where


import Data.Text (pack)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder as L
import Data.Text.Lazy.Builder.Int as L
import Text.XML.Generator
import Data.Types
import qualified Data.ByteString.Lazy as B



docInfo :: DocInfo
docInfo = defaultDocInfo


convertToInstrumentXML :: InstrumentFile -> B.ByteString
convertToInstrumentXML (InstrumentFile{..}) = xrender $
    doc docInfo $
        xelem "instrument" (xattr "version" ifVersion <> xattr "name" ifName <#> xelem "samples" (samples ifSamples))

samples :: [HitSample] -> Xml Elem
samples ss =
    xelems $ map sampleNode ss
    where
        sampleNode x =
            xelem "sample" (xattr "name" (hsName x) <> xattr "power" ((pack.show.hsPower) x) <#> (audiofiles (hsSamples x)))

audiofiles :: [AudioFile] -> Xml Elem
audiofiles afs =
    xelems $ map audioF afs
    where
        audioF x =
            xelem "audiofile" (xattr "channel" (pack (show (afChannel x)))
                            <> xattr "file" (pack (afPath x))
                            <> xattr "filechannel" ((pack.show.afFileChannel) x))


convertToMidiMapXML :: MidiMap -> B.ByteString
convertToMidiMapXML (MidiMap mp) = xrender $
    doc docInfo $
        xelem "midimap" (xelems (map notes mp))
    where
        notes (i, inst) = xelem "map" (xattr "note" (pack (show i)) <> xattr "instr" inst)



convertToDrumkitXML :: Drumkit -> B.ByteString
convertToDrumkitXML dr = xrender $
    doc docInfo $
        xelem "drumkit" (xattr "name" (dkName dr) <> xattr "description" (dkDescription dr) <#> channels <> instruments)
    where
        channels = xelem "channels" (xelems (map ch (dkChannels dr)))
        ch x = xelem "channel" (xattr "name" (pack (show x)))
        instruments = xelem "instruments" (xelems (map ins (dkInstruments dr)))
        ins x = xelem "instrument" (xattr "name" (cmName x) <> gr x <> xattr "file" (pack (cmFile x)) <#> channelmap x)
        gr x = case cmGroup x of
                    Just g -> xattr "group" g
                    Nothing -> mempty
        channelmap x = xelems (map chm (cmMap x))
        chm (c1, c2) = xelem "channelmap" (xattr "in" c1 <> xattr "out" c2)


convertToTabSep :: MidiMap -> L.Text
convertToTabSep mm =
    let ls = mmNote mm
        f (note, inst) b = fromText inst <> fromText "\t" <> decimal note <> fromText "\t" <> fromText (midiToNote note) <> fromText "\n" <> b
        header = "Instrument\tMIDI\tNote\n"
    in
    toLazyText $ header <> foldr f (fromText "") ls

