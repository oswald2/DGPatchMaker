{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.Export
    (
    convertToInstrumentXML
    )
where


import Data.Text (pack)
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
            xelem "audiofile" (xattr "channel" (afChannel x)
                            <> xattr "file" (pack (afPath x))
                            <> xattr "filechannel" ((pack.show.afFileChannel) x))


