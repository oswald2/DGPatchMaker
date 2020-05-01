{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.Import
  ( importMidiMap
  , importInstrumentFile
  , importDrumkitFile
  )
where


import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , append
                                                )
import           Data.Text.Read
import           Data.Types
import           Text.XML.Stream.Parse
import           Data.XML.Types
import qualified Data.Vector                   as V
import           Data.Either
import           Data.Maybe
import           System.FilePath

import           Control.Monad.Trans.Resource
import           Data.Conduit

import           Control.Exception
import           Data.Typeable


data DKParseException =
    AudioFileParseError { dkpeMsg :: Text }
    | SampleFileParseError { dkpeMsg :: Text }
    | DrumkitParseError { dkpeMsg :: Text }
    deriving (Show, Typeable)

instance Exception DKParseException



parseMidiNote :: MonadThrow m => ConduitM Event o m (Maybe (Int, Text))
parseMidiNote = tag' "map" ((,) <$> requireAttr "note" <*> requireAttr "instr")
  $ \(note, instr) -> return (read (unpack note), instr)


parseMidiMap :: MonadThrow m => ConduitM Event o m (Maybe MidiMap)
parseMidiMap = do
  mms <- tagNoAttr "midimap" $ many parseMidiNote
  case mms of
    Nothing -> return Nothing
    Just x  -> return $ Just (MidiMap x)


importMidiMap :: FilePath -> IO (Maybe MidiMap)
importMidiMap path = do
  runConduitRes $ parseFile def path .| parseMidiMap




parseInstrument
  :: MonadThrow m => FilePath -> ConduitM Event o m (Maybe InstrumentFile)
parseInstrument fname = do
  inst <-
    tag' "instrument" ((,) <$> requireAttr "version" <*> requireAttr "name")
      $ \(version, name) -> do
          smpls <- tagNoAttr "samples" $ many parseSamples
          return (version, name, smpls)
  case inst of
    Just (vers, nam, Just smpl) ->
      return $ Just (InstrumentFile vers nam (pack fname) Nothing smpl)
    _ -> return Nothing



parseSamples :: MonadThrow m => ConduitM Event o m (Maybe HitSample)
parseSamples =
  tag' "sample" ((,) <$> requireAttr "name" <*> requireAttr "power")
    $ \(name, power) -> do
        af <- many parseAudioFile
        let p = double power
        if isLeft p
          then throwM
            (SampleFileParseError
              ("Invalid Power specified for Sample: " `append` name)
            )
          else do
            let Right (x, _) = p
            return (HitSample name x af)


parseAudioFile :: MonadThrow m => ConduitM Event o m (Maybe AudioFile)
parseAudioFile = do
  tag' "audiofile" attrs $ \af -> return af
 where
  attrs = do
    chan        <- requireAttr "channel"
    file        <- requireAttr "file"
    filechannel <- requireAttr "filechannel"
    let filechannel' = decimal filechannel

    if isLeft filechannel'
      then throwM
        (AudioFileParseError ("Invalid Filechannel for file: " `append` file))
      else do
        let Right (x, _) = filechannel'
        return $ AudioFile chan (unpack file) x Nothing Nothing


importInstrumentFile :: FilePath -> IO (Either Text InstrumentFile)
importInstrumentFile path = do
  catches worker [Handler handler, Handler handler2]
 where
  worker = do
    iF <- runConduitRes $ parseFile def path .| parseInstrument
      (takeFileName path)
    return (maybe (Left "Could not parse file") Right iF)
  handler e = return (Left (dkpeMsg e))
  handler2 XmlException {..} = do
    let msg = pack xmlErrorMessage `append` "\n\nContext: " `append` pack
          (show xmlBadInput)
    return (Left msg)
  handler2 e = return (Left (pack (show e)))


conduitDrumKitXML :: MonadThrow m => ConduitM Event o m (Maybe Drumkit)
conduitDrumKitXML = do
  tag' "drumkit"
       ((,,) <$> attr "name" <*> attr "description" <*> attr "samplerate")
    $ \(name, description, samplerate) -> do
        meta  <- conduitMeta
        chans <- channels
        insts <- instruments
        case (chans, insts) of
          (Just c, Just i) ->
            return $ generateDrumKit name description meta samplerate c i
          _ -> throwM (DrumkitParseError "Cannot parse drumkit")
 where
  channels    = tagNoAttr "channels" (many ch)
  ch          = tag' "channel" (requireAttr "name") return
  instruments = tagNoAttr "instruments" (many instrumentData)



instrumentData
  :: (Monad m, MonadThrow m) => ConduitM Event o m (Maybe ChannelMap)
instrumentData =
  tag' "instrument"
       ((,,) <$> requireAttr "name" <*> attr "group" <*> requireAttr "file")
    $ \(name, group, file) -> do
        chokes <- chokeData
        cm'    <- many channelmap
        let cm = V.fromList $ map mkChannelMapItemTuple cm'
        return $ ChannelMap name
                            group
                            (unpack file)
                            Nothing
                            cm
                            (cmCheckUndefined cm)
                            (mapChokes chokes)
 where
  channelmap = tag'
    "channelmap"
    ((,,) <$> requireAttr "in" <*> requireAttr "out" <*> attr "main")
    return
  mapChokes Nothing = Disabled [] 
  mapChokes (Just chokes) = Enabled chokes


chokeData :: (Monad m, MonadThrow m) => ConduitM Event o m (Maybe [ChokeData])
chokeData = tagNoAttr "chokes" $ many chokes
 where
  chokes =
    tag' "choke" ((,) <$> requireAttr "instrument" <*> requireAttr "choketime")
      $ \(instr, t) -> case signed decimal t of
          Left err -> throwM
            (DrumkitParseError
              ("Error parsing choke: " <> t <> " is not an integer with:" <> (pack err)
              )
            )
          Right (time, _) -> return $ ChokeData instr time



generateDrumKit
  :: Maybe Text
  -> Maybe Text
  -> Maybe MetaData
  -> Maybe Text
  -> [Text]
  -> [ChannelMap]
  -> Drumkit
generateDrumKit _name _descr (Just meta) samplerate channels instrs =
  Drumkit (Right meta) samplerate channels instrs
generateDrumKit name descr Nothing samplerate channels instrs =
  let oldDescr = OldDescr (fromMaybe "" name) (fromMaybe "" descr)
  in  Drumkit (Left oldDescr) samplerate channels instrs


conduitMeta :: (Monad m, MonadThrow m) => ConduitM Event o m (Maybe MetaData)
conduitMeta = tagNoAttr "metadata" $ do
  v    <- version
  t    <- title
  l    <- logo
  desc <- description
  lic  <- license
  n    <- notes
  auth <- author
  em   <- email
  ws   <- website
  im  <- imageData

  return MetaData { metaVersion     = v
                  , metaTitle       = t
                  , metaLogo        = l
                  , metaDescription = desc
                  , metaLicense     = lic
                  , metaNotes       = n
                  , metaAuthor      = auth
                  , metaEMail       = em
                  , metaWebsite     = ws
                  , metaImage       = im
                  }
 where
  version     = tagNoAttr "version" content
  title       = tagNoAttr "title" content
  logo        = tag' "logo" (requireAttr "src") pure
  description = tagNoAttr "description" content
  license     = tagNoAttr "license" content
  notes       = tagNoAttr "notes" content
  author      = tagNoAttr "author" content
  email       = tagNoAttr "email" content
  website     = tagNoAttr "website" content


imageData :: (Monad m, MonadThrow m) => ConduitM Event o m (Maybe ImageData)
imageData =
  tag' "image" ((,) <$> requireAttr "src" <*> requireAttr "map")
    $ \(src, mapImg) -> do
        items <- many clickMapItem
        return (ImageData src mapImg items)

clickMapItem
  :: (Monad m, MonadThrow m) => ConduitM Event o m (Maybe ClickMapItem)
clickMapItem =
  tag' "clickmap"
       (ClickMapItem <$> requireAttr "instrument" <*> requireAttr "colour")
    $ \item -> return item



importDrumkitFile :: FilePath -> IO (Either Text Drumkit)
importDrumkitFile path = do
  catches worker [Handler handler, Handler handler2]
 where
  worker = do
    iF <- runConduitRes $ parseFile def path .| conduitDrumKitXML
    return (maybe (Left "Could not parse file") Right iF)
  handler e = return (Left (dkpeMsg e))
  handler2 XmlException {..} = do
    let msg = pack xmlErrorMessage `append` "\n\nContext: " `append` pack
          (show xmlBadInput)
    return (Left msg)
  handler2 e = return (Left (pack (show e)))

