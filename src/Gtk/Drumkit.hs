{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.Drumkit
  ( DrumkitPage
  , initDrumkitPage
  , setDkSampleRate
  , setDkMetaData
  , getDkMetaData
  )
where


--import           ClassyPrelude

import           System.FilePath.Find          as F
import           System.FilePath

import qualified Data.Vector                   as V
import           Data.Maybe

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Types
import           Data.DrumDrops.Utils
import           Data.Char                      ( isSpace )
import           Data.Export
import           Data.Drumgizmo
import           Data.Import
import           Data.Either
import           Data.List                     as L
                                                ( find
                                                , sortOn
                                                )
import           Data.Version


import           Data.Vector                    ( Vector )
--import qualified Data.Vector                   as V
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
--import Data.Text as T (pack)
import           Data.IORef
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
--import           Data.Checkers
import           Data.Defaults


import           Graphics.UI.Gtk               as G

import           Gtk.Utils
import           Gtk.Colors
import           Gtk.InstrumentFrame
import           Gtk.ErrorDialog
import           Gtk.MidiMap
import           Gtk.FileHandlingDialog
import           Gtk.DirectedChokeDialog
import           Gtk.ClickyKitDialog



data DrumkitPage = DrumkitPage {
    guiDkParentWindow :: Window,
    guiDkName :: Entry,
    guiDkDescription :: TextView,
    guiDkSampleRate :: Entry,
    guiBaseDir :: Entry,
    guiSamplesDir :: Entry,
    guiBtImportDrumkit :: Button,
    guiBtSetBaseDir :: Button,
    guiBtSetSamplesDir :: Button,
    guiDkInstrumentsNotebook :: Notebook,
    guiDkProgress :: ProgressBar,
    guiTvChannels :: TreeView,
    guiTvInstruments :: TreeView,
    guiTvChannelMap :: TreeView,
    guiTvChannelsModel :: ListStore Text,
    guiTvInstrumentsModel :: ListStore ChannelMap,
    guiTvChannelMapModel :: ListStore ChannelMapItem,
    guiDrumkit :: IORef (Maybe Drumkit),
    guiDkInstrumentPages :: IORef (Vector InstrumentPage),
    guiErrDiag :: ErrorDialog,
    guiMidiMapGM :: MidiMapPage,
    guiMidiMapDef :: MidiMapPage,
    guiParserCombo :: ComboBox,
    guiChannelMenu :: Menu,
    guiChannelRenderer :: CellRendererText,
    guiFhDialog :: FileHandlingDialog,
    guiOutChannelRenderer :: CellRendererCombo,
    guiGroupRenderer :: CellRendererText,
    guiChannelMapMenu :: Menu,
    guiBtCompileGM :: Button,
    guiBtCompileDef :: Button,
    guiSelectedChannelMap :: IORef (Maybe (Int, ChannelMap)),
    guiMetaVersion :: Entry,
    guiMetaTitle :: Entry,
    guiMetaLogo :: Entry,
    guiMetaDescription :: TextView,
    guiMetaLicense :: TextView,
    guiMetaNotes :: TextView,
    guiMetaAuthor :: Entry,
    guiMetaEMail :: Entry,
    guiMetaWebSite :: Entry,
    guiMetaEnable :: CheckButton,
    guiMetaNotebook :: Notebook,
    guiButtonEditChokes :: Button,
    guiDirectedChokeDialog :: DirectedChokeDialog,
    guiClickyKitDialog :: ClickyKitDialog,
    guiClickyKitCheck :: CheckButton,
    guiButtonEditClickyKit :: Button
}


initDrumkitPage
  :: Window
  -> G.Builder
  -> Notebook
  -> ProgressBar
  -> ComboBox
  -> Entry
  -> Entry
  -> IORef (Vector InstrumentPage)
  -> FileHandlingDialog
  -> IO DrumkitPage
initDrumkitPage mainWindow builder instrumentsNotebook progress combo entryBaseDirectory entrySamplesDir ioref fhDialog
  = do

    buttonImportDrumkit <- builderGetObject builder
                                            castToButton
                                            ("buttonImportDrumkit" :: Text)
    -- buttonExportDrumkit <- builderGetObject builder castToButton ("buttonExportDrumkit" :: Text)
    buttonSetBaseDir <- builderGetObject builder
                                         castToButton
                                         ("buttonSetBaseDir" :: Text)
    buttonSetSamplesDir <- builderGetObject builder
                                            castToButton
                                            ("buttonSetSamplesDir" :: Text)
    buttonConvertToFullMix <- builderGetObject
      builder
      castToButton
      ("buttonConvertToFullMix" :: Text)

    miLoadDrumKit <- builderGetObject builder
                                      castToMenuItem
                                      ("imagemenuitemLoadDrumkit" :: Text)
    miExportDrumKit <- builderGetObject builder
                                        castToMenuItem
                                        ("menuitemExportDrumkit" :: Text)
    miSaveDrumkitFile <- builderGetObject
      builder
      castToMenuItem
      ("imagemenuitemSaveDrumkitFile" :: Text)

    itemShowAbout <- builderGetObject builder
                                      castToMenuItem
                                      ("menuItemAbout" :: Text)
    aboutDialog <- builderGetObject builder
                                    castToAboutDialog
                                    ("aboutDialog" :: Text)

    tvChannels <- builderGetObject builder
                                   castToTreeView
                                   ("treeviewChannels" :: Text)
    tvInstruments <- builderGetObject builder
                                      castToTreeView
                                      ("treeviewInstruments" :: Text)
    tvChannelMap <- builderGetObject builder
                                     castToTreeView
                                     ("treeviewChannelMap" :: Text)

    eName <- builderGetObject builder castToEntry ("entryDkName" :: Text)
    eSr   <- builderGetObject builder castToEntry ("entrySampleRate" :: Text)
    eDesc <- builderGetObject builder
                              castToTextView
                              ("textviewDkDescription" :: Text)

    tvMidiGM <- builderGetObject builder
                                 castToTreeView
                                 ("treeviewMidiMapGM" :: Text)
    tvMidiDef <- builderGetObject builder
                                  castToTreeView
                                  ("treeviewMidiMapDef" :: Text)


    metaenable <- builderGetObject builder
                                   castToCheckButton
                                   ("checkButtonMeta" :: Text)
    evers   <- builderGetObject builder castToEntry ("entryMetaVersion" :: Text)
    etitle  <- builderGetObject builder castToEntry ("entryMetaTitle" :: Text)
    elogo   <- builderGetObject builder castToEntry ("entryMetaLogo" :: Text)
    tvdescr <- builderGetObject builder
                                castToTextView
                                ("textviewMetaDescription" :: Text)
    tvlicense <- builderGetObject builder
                                  castToTextView
                                  ("textviewMetaLicense" :: Text)
    tvnotes <- builderGetObject builder
                                castToTextView
                                ("textviewMetaNotes" :: Text)
    eauthor  <- builderGetObject builder castToEntry ("entryMetaAuthor" :: Text)
    eemail   <- builderGetObject builder castToEntry ("entryMetaEMail" :: Text)
    ewebsite <- builderGetObject builder
                                 castToEntry
                                 ("entryMetaWebsite" :: Text)

    metaNotebook <- builderGetObject builder
                                     castToNotebook
                                     ("notebookMetaData" :: Text)

    buttonEditChokes <- builderGetObject builder
                                         castToButton
                                         ("buttonEditChokes" :: Text)

    lsm             <- listStoreNew []
    lsinst          <- listStoreNew []
    lscm            <- listStoreNew []

    errDiag         <- initErrorDialog builder

    dr              <- newIORef Nothing
    selIoRef        <- newIORef Nothing

    channelRenderer <- initTvChannels tvChannels lsm
    groupRenderer   <- initTvInstruments tvInstruments lsinst
    outRenderer     <- initTvChannelMap tvChannelMap lsm lscm lsinst selIoRef

    chokeDialog     <- initDialog mainWindow builder
    clickyKitDialog <- initClickyKitDialog mainWindow builder entryBaseDirectory

    -- entrySetText entryBaseDirectory ("/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Multi Velocity Pack" :: FilePath)
    -- entrySetText entrySamplesDir ("/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Multi Velocity Pack/SAMPLES" :: FilePath)

    -- entrySetText eName ("MapexHeavyRockMultiVelocity" :: Text)
    entrySetText entryBaseDirectory ("" :: FilePath)
    entrySetText entrySamplesDir    ("" :: FilePath)
    entrySetText eName              ("" :: Text)

    gmLoadButton <- builderGetObject builder
                                     castToButton
                                     ("buttonLoadGMMap" :: Text)
    gmExportButton <- builderGetObject builder
                                       castToButton
                                       ("buttonExportGMMap" :: Text)

    defLoadButton <- builderGetObject builder
                                      castToButton
                                      ("buttonLoadDefMap" :: Text)
    defExportButton <- builderGetObject builder
                                        castToButton
                                        ("buttonExportDefMap" :: Text)

    resetButton <- builderGetObject builder castToButton ("buttonReset" :: Text)
    compileButton <- builderGetObject builder
                                      castToButton
                                      ("buttonCompile" :: Text)

    midiMapGm <- initMidiMap mainWindow
                             tvMidiGM
                             entryBaseDirectory
                             gmLoadButton
                             gmExportButton
                             fhDialog
    midiMapDef <- initMidiMap mainWindow
                              tvMidiDef
                              entryBaseDirectory
                              defLoadButton
                              defExportButton
                              fhDialog

    initParserCombo combo

    channelMenu <- builderGetObject builder castToMenu ("menuChannels" :: Text)
    itemAddChannel <- builderGetObject builder
                                       castToMenuItem
                                       ("menuitemAddChannel" :: Text)
    itemRemoveChannel <- builderGetObject builder
                                          castToMenuItem
                                          ("menuitemRemoveChannel" :: Text)

    channelMapMenu <- builderGetObject builder
                                       castToMenu
                                       ("menuChannelMap" :: Text)
    itemDuplicate <- builderGetObject builder
                                      castToMenuItem
                                      ("menuitemDuplicate" :: Text)
    itemRemoveCM <- builderGetObject builder
                                     castToMenuItem
                                     ("menuitemRemove" :: Text)

    btCompileGM <- builderGetObject builder
                                    castToButton
                                    ("buttonCompileFromKitGM" :: Text)
    btCompileDef <- builderGetObject builder
                                     castToButton
                                     ("buttonCompileFromKitDefault" :: Text)

    btUp      <- builderGetObject builder castToButton ("buttonUp" :: Text)
    btDown    <- builderGetObject builder castToButton ("buttonDown" :: Text)
    btSort    <- builderGetObject builder castToButton ("buttonSort" :: Text)

    chkClicky <- builderGetObject builder
                                  castToCheckButton
                                  ("checkbuttonClickyKit" :: Text)
    btClicky <- builderGetObject builder
                                 castToButton
                                 ("buttonShowClickyKit" :: Text)

    let gui = DrumkitPage { guiDkParentWindow        = mainWindow
                          , guiTvChannels            = tvChannels
                          , guiTvInstruments         = tvInstruments
                          , guiTvChannelMap          = tvChannelMap
                          , guiTvChannelsModel       = lsm
                          , guiTvInstrumentsModel    = lsinst
                          , guiTvChannelMapModel     = lscm
                          , guiDrumkit               = dr
                          , guiDkName                = eName
                          , guiDkDescription         = eDesc
                          , guiDkSampleRate          = eSr
                          , guiBaseDir               = entryBaseDirectory
                          , guiSamplesDir            = entrySamplesDir
                          , guiBtImportDrumkit       = buttonImportDrumkit
                          , guiBtSetBaseDir          = buttonSetBaseDir
                          , guiBtSetSamplesDir       = buttonSetSamplesDir
                          , guiDkInstrumentsNotebook = instrumentsNotebook
                          , guiDkProgress            = progress
                          , guiErrDiag               = errDiag
                          , guiDkInstrumentPages     = ioref
                          , guiMidiMapGM             = midiMapGm
                          , guiMidiMapDef            = midiMapDef
                          , guiParserCombo           = combo
                          , guiChannelMenu           = channelMenu
                          , guiChannelRenderer       = channelRenderer
                          , guiFhDialog              = fhDialog
                          , guiOutChannelRenderer    = outRenderer
                          , guiGroupRenderer         = groupRenderer
                          , guiChannelMapMenu        = channelMapMenu
                          , guiBtCompileGM           = btCompileGM
                          , guiBtCompileDef          = btCompileDef
                          , guiSelectedChannelMap    = selIoRef
                          , guiMetaVersion           = evers
                          , guiMetaTitle             = etitle
                          , guiMetaLogo              = elogo
                          , guiMetaDescription       = tvdescr
                          , guiMetaLicense           = tvlicense
                          , guiMetaNotes             = tvnotes
                          , guiMetaAuthor            = eauthor
                          , guiMetaEMail             = eemail
                          , guiMetaWebSite           = ewebsite
                          , guiMetaEnable            = metaenable
                          , guiMetaNotebook          = metaNotebook
                          , guiButtonEditChokes      = buttonEditChokes
                          , guiDirectedChokeDialog   = chokeDialog
                          , guiClickyKitDialog       = clickyKitDialog
                          , guiClickyKitCheck        = chkClicky
                          , guiButtonEditClickyKit   = btClicky
                          }
    --setDkDescription gui "Mapex Heavy Rock Kit patch from the All Samples Pack from DrumDrops (http://www.drumdrops.com). Created by M. Oswald."
    setDkDescription gui ""

    void $ G.on buttonSetBaseDir buttonActivated $ setBaseDir gui
    void $ G.on buttonSetSamplesDir buttonActivated $ setSamplesDir gui

    void $ G.on buttonImportDrumkit buttonActivated $ importDrumDropsDrumKit gui
    --void $ G.on buttonExportDrumkit buttonActivated $ exportDrumKit gui
    --void $ G.on buttonLoadDrumkit buttonActivated $ loadDrumkit gui

    void $ G.on buttonConvertToFullMix buttonActivated $ convertToFullMix gui

    void $ G.on miLoadDrumKit menuItemActivated $ loadDrumkit gui
    void $ G.on miExportDrumKit menuItemActivated $ exportDrumKit gui
    void $ G.on miSaveDrumkitFile menuItemActivated $ saveDrumkit gui

    void $ G.on resetButton buttonActivated $ resetDrumkit gui
    void $ G.on compileButton buttonActivated $ compileDrumkit gui

    void $ G.on itemAddChannel menuItemActivated $ addChannel gui
    void $ G.on itemRemoveChannel menuItemActivated $ removeChannel gui

    void $ G.on itemDuplicate menuItemActivated $ duplicateCM gui
    void $ G.on itemRemoveCM menuItemActivated $ removeCM gui

    void $ G.on btCompileGM buttonActivated $ compileMidiMapGM gui
    void $ G.on btCompileDef buttonActivated $ compileMidiMapDefault gui

    void $ G.on btUp buttonActivated $ channelUp gui
    void $ G.on btDown buttonActivated $ channelDown gui
    void $ G.on btSort buttonActivated $ sortChannels gui

    void $ G.on metaenable toggled $ do
      ena <- toggleButtonGetActive metaenable
      notebookSetCurrentPage metaNotebook (if ena then 1 else 0)

    set
      aboutDialog
      [ aboutDialogProgramName := ("DGPatchMaker" :: Text)
      , aboutDialogVersion := versionString
      ]

    void $ G.on itemShowAbout menuItemActivated $ do
      widgetShow aboutDialog
      void $ dialogRun aboutDialog
      widgetHide aboutDialog
      return ()

    setupCallbacks gui

    return gui


getDkName :: DrumkitPage -> IO Text
getDkName dkp = entryGetText (guiDkName dkp)


getMetaDkName :: DrumkitPage -> IO Text 
getMetaDkName dkp = entryGetText (guiMetaTitle dkp)

getName :: DrumkitPage -> IO (Maybe Text)
getName dkp = do 
  nm <- getMetaDkName dkp 
  if T.null nm 
    then do 
      nm2 <- getDkName dkp 
      if T.null nm2 then return Nothing 
      else return (Just nm2)
    else return (Just nm)



getDkDescription :: DrumkitPage -> IO Text
getDkDescription dkp = do
  buffer       <- textViewGetBuffer (guiDkDescription dkp)
  (start, end) <- textBufferGetBounds buffer
  res          <- textBufferGetText buffer start end False
  return $ T.filter (/= '\n') res

getDkSampleRateText :: DrumkitPage -> IO Text
getDkSampleRateText dkp = entryGetText (guiDkSampleRate dkp)

getDkSampleRate :: DrumkitPage -> IO Int
getDkSampleRate dkp = do
  sr <- entryGetText (guiDkSampleRate dkp)
  if T.null sr then return defaultSampleRate else return (read (T.unpack sr))


setDkSampleRate :: DrumkitPage -> Int -> IO ()
setDkSampleRate dkp sr = entrySetText (guiDkSampleRate dkp) (T.pack (show sr))


setDkSampleRateText :: DrumkitPage -> Text -> IO ()
setDkSampleRateText dkp = entrySetText (guiDkSampleRate dkp)


setDkDescription :: DrumkitPage -> Text -> IO ()
setDkDescription dkp desc = do
  buffer <- textViewGetBuffer (guiDkDescription dkp)
  textBufferSetText buffer desc

setDkMetaData :: DrumkitPage -> Maybe MetaData -> IO ()
setDkMetaData dkp Nothing = do
  toggleButtonSetActive (guiMetaEnable dkp) False
setDkMetaData dkp (Just m) = do
  toggleButtonSetActive (guiMetaEnable dkp) True
  entrySetText (guiMetaVersion dkp) $ fromMaybe "" (metaVersion m)
  entrySetText (guiMetaTitle dkp) $ fromMaybe "" (metaTitle m)
  entrySetText (guiMetaLogo dkp) $ fromMaybe "" (metaLogo m)
  entrySetText (guiMetaAuthor dkp) $ fromMaybe "" (metaAuthor m)
  entrySetText (guiMetaEMail dkp) $ fromMaybe "" (metaEMail m)
  entrySetText (guiMetaWebSite dkp) $ fromMaybe "" (metaWebsite m)

  textViewGetBuffer (guiMetaDescription dkp)
    >>= \buf -> textBufferSetText buf $ fromMaybe "" (metaDescription m)
  textViewGetBuffer (guiMetaLicense dkp)
    >>= \buf -> textBufferSetText buf $ fromMaybe "" (metaLicense m)
  textViewGetBuffer (guiMetaNotes dkp)
    >>= \buf -> textBufferSetText buf $ fromMaybe "" (metaNotes m)

  toggleButtonSetActive (guiClickyKitCheck dkp) $ isJust (metaImage m)


getDkMetaData :: DrumkitPage -> IO MetaData
getDkMetaData dkp = do
  vers    <- getTxt (guiMetaVersion dkp)
  title   <- getTxt (guiMetaTitle dkp)
  logo    <- getTxt (guiMetaLogo dkp)
  author  <- getTxt (guiMetaAuthor dkp)
  email   <- getTxt (guiMetaEMail dkp)
  website <- getTxt (guiMetaWebSite dkp)

  descr   <- getTxtV (guiMetaDescription dkp)
  license <- getTxtV (guiMetaLicense dkp)
  notes   <- getTxtV (guiMetaNotes dkp)

  dk' <- readIORef (guiDrumkit dkp)
  let img = case dk' of 
        Nothing -> Nothing 
        Just dk -> case dkInfo dk of 
          Left _ -> Nothing 
          Right meta -> metaImage meta 

  let !newMeta = MetaData { metaVersion     = vers
                  , metaTitle       = title
                  , metaLogo        = logo
                  , metaDescription = descr
                  , metaLicense     = license
                  , metaNotes       = notes
                  , metaAuthor      = author
                  , metaEMail       = email
                  , metaWebsite     = website
                  , metaImage       = img
                  }
  return newMeta
 where
  toMaybe txt = if T.null txt then Nothing else Just txt
  getTxt w = toMaybe <$> entryGetText w

  getTxtV w = toMaybe <$> textViewGetText w


initParserCombo :: ComboBox -> IO ()
initParserCombo cb = do
  void $ comboBoxSetModelText cb
  mapM_ (comboBoxAppendText cb) str
  comboBoxSetActive cb 0
  where str = map (T.pack . show) (enumFrom MapexParser)

setBaseDir :: DrumkitPage -> IO ()
setBaseDir mainWindow = do
  let parentWindow = guiDkParentWindow mainWindow
  dialog <- fileChooserDialogNew
    (Just ("Set Base Directory for Imports" :: Text))             --dialog title
    (Just parentWindow)                     --the parent window
    FileChooserActionSelectFolder                         --the kind of dialog we want
    [ ( "gtk-cancel"                                --The buttons to display
      , ResponseCancel
      )
    , ("gtk-open", ResponseAccept)
    ]

  basepath <- entryGetText (guiBaseDir mainWindow)
  void $ fileChooserSetFilename dialog basepath

  widgetShow dialog
  resp <- dialogRun dialog
  case resp of
    ResponseAccept -> do
      f <- fileChooserGetFilename dialog
      case f of
        Nothing  -> return ()
        Just dir -> do
          entrySetText (guiBaseDir mainWindow) dir
          txt <- entryGetText (guiSamplesDir mainWindow)
          when (T.null txt) $ entrySetText (guiSamplesDir mainWindow) dir
    ResponseCancel      -> return ()
    ResponseDeleteEvent -> return ()
    _                   -> return ()
  widgetHide dialog


setSamplesDir :: DrumkitPage -> IO ()
setSamplesDir mainWindow = do
  let parentWindow = guiDkParentWindow mainWindow
  dialog <- fileChooserDialogNew
    (Just ("Set Sample Base Directory for Imports" :: Text))             --dialog title
    (Just parentWindow)                     --the parent window
    FileChooserActionSelectFolder                         --the kind of dialog we want
    [ ( "gtk-cancel"                                --The buttons to display
      , ResponseCancel
      )
    , ("gtk-open", ResponseAccept)
    ]
  loc <- entryGetText (guiSamplesDir mainWindow)
  void $ fileChooserSetFilename dialog loc

  widgetShow dialog
  resp <- dialogRun dialog
  case resp of
    ResponseAccept -> do
      f <- fileChooserGetFilename dialog
      case f of
        Nothing  -> return ()
        Just dir -> do
          entrySetText (guiSamplesDir mainWindow) dir
          return ()
    ResponseCancel      -> return ()
    ResponseDeleteEvent -> return ()
    _                   -> return ()
  widgetHide dialog



setDrumkit :: DrumkitPage -> Drumkit -> IO ()
setDrumkit gui dk = do
  writeIORef (guiDrumkit gui) (Just dk)
  widgetSetSensitive (guiBtCompileGM gui)  True
  widgetSetSensitive (guiBtCompileDef gui) True


clearDrumkit :: DrumkitPage -> IO ()
clearDrumkit gui = do
  writeIORef (guiDrumkit gui) Nothing
  widgetSetSensitive (guiBtCompileGM gui)  False
  widgetSetSensitive (guiBtCompileDef gui) False
  setDkMetaData gui (Just clearMetaData)


getDrumkit :: DrumkitPage -> IO (Maybe Drumkit)
getDrumkit gui = do
  readIORef (guiDrumkit gui)


importDrumDropsDrumKit :: DrumkitPage -> IO ()
importDrumDropsDrumKit gui = do
  widgetSetSensitive (guiParserCombo gui) False
  catch
    (importDrumDropsDrumKit' gui)
    (\e -> displayErrorBox (guiDkParentWindow gui)
                           ("Error: " <> T.pack (show (e :: SomeException)))
    )
  widgetSetSensitive (guiParserCombo gui) True



importDrumDropsDrumKit' :: DrumkitPage -> IO ()
importDrumDropsDrumKit' gui = do
  b <- entryGetText (guiBaseDir gui) :: IO FilePath
  case b of
    ""      -> displayErrorBox (guiDkParentWindow gui) "Basedir must be set!"
    basedir -> do
      samplesDir <- entryGetText (guiSamplesDir gui)

      dirs       <- getDirectoriesToImport samplesDir

      -- first clear the instruments notebook
      clearNotebook (guiDkInstrumentsNotebook gui)

      instFiles <- doImport basedir samplesDir dirs

      let errs = lefts instFiles
      if not (null errs)
        then do
          displayMultiErrors
            (guiErrDiag gui)
            "Multiple errors happened during Import of Instruments:"
            errs
          return ()
        else do
          let
            insts :: [(InstrumentFile, Int)]
            insts      = rights instFiles
            sampleRate = if null insts
              then 44100
              else let (_, sr) = Prelude.head insts in sr
              --sampleRate = 44100
          info <- getInfo gui

          let drumkit = generateDrumkit info
                                        (Just (T.pack (show sampleRate)))
                                        (map fst insts)

          -- set the actual drumkit
          setDrumkit gui drumkit

          -- set the channels for viewing
          setChannels gui (dkChannels drumkit)
          setInstruments gui (dkInstruments drumkit)

          -- also convert the drumkit to a midi map
          let midimap = getMidiMap drumkit

          setMidiMap (guiMidiMapGM gui)  midimap
          setMidiMap (guiMidiMapDef gui) midimap

          return ()
      return ()
 where
  doImport
    :: FilePath
    -> FilePath
    -> [FilePath]
    -> IO [Either Text (InstrumentFile, Int)]
  doImport basedir samplesDir paths = do
      -- import the instruments
    let progress = guiDkProgress gui
        n        = length paths
        step :: Double
        step = 1.0 / fromIntegral n
    progressBarSetText progress ("Importing DrumDrops Drumkit..." :: Text)

    instruments <- forM paths (doSingleImport progress basedir samplesDir step)

    progressBarSetText progress ("" :: Text)
    progressBarSetFraction progress 0.0

    return instruments

  doSingleImport progress basedir samplesDir step path = do
    ins <- instrumentPageNew (guiDkParentWindow gui)
                             (guiDkInstrumentsNotebook gui)
                             (guiBaseDir gui)
                             (guiSamplesDir gui)
                             (guiParserCombo gui)
                             (guiDkInstrumentPages gui)
                             (guiFhDialog gui)
                             (guiErrDiag gui)
                             (setDkSampleRate gui)
    let instName = pathToInstrument samplesDir path
    _ <- notebookAppendPage (guiDkInstrumentsNotebook gui)
                            (instrumentPageGetMainBox ins)
                            instName
    instrumentPageInsert ins

    pt <- comboBoxGetActiveText (guiParserCombo gui)
    let parserType = maybe MapexParser (read . T.unpack) pt

    res <- importInstrument parserType basedir samplesDir path
    case res of
      Left err -> do
        displayErrorBox (guiDkParentWindow gui) err
        return (Left err)
      Right r@(instFile, _sampleRate) -> do
        instrumentPageSetInstrumentFile ins instFile

        -- update the progress bar
        frac <- progressBarGetFraction progress
        progressBarSetFraction progress (frac + step)

        yield

        return (Right r)
  yield = do
    i <- eventsPending
    when (i > 0) $ do
      void mainIteration
      yield




getDirectoriesToImport :: FilePath -> IO [FilePath]
getDirectoriesToImport path = do
  dirs <- F.find recP filterP path
  let s :: Set FilePath
      s = S.fromList $ map takeDirectory dirs
  return (S.toList s)
 where
  recP    = always
  filterP = extension ==? ".wav"


initTvChannels :: TreeView -> ListStore Text -> IO CellRendererText
initTvChannels tv ls = do
  treeViewSetModel tv (Just ls)

  treeViewSetHeadersVisible tv True

  -- add a couple columns
  col1 <- treeViewColumnNew

  treeViewColumnSetTitle col1 ("Channels" :: Text)

  renderer1 <- cellRendererTextNew

  cellLayoutPackStart col1 renderer1 True

  set
    renderer1
    [ cellTextEditable := True
    , cellTextEditableSet := True
    , cellTextBackgroundColor := paleYellow
    , cellTextBackgroundSet := True
    ]

  cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [cellText := hs]

  _ <- treeViewAppendColumn tv col1

  treeViewSetEnableSearch tv True
  treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
    (i : _) <- treeModelGetPath ls iter
    row     <- listStoreGetValue ls i
    return $ T.toLower str `T.isPrefixOf` T.toLower (T.pack (show row))

  return (renderer1)


setChannels :: DrumkitPage -> [Text] -> IO ()
setChannels gui = setListStoreTo (guiTvChannelsModel gui)


initTvInstruments :: TreeView -> ListStore ChannelMap -> IO CellRendererText
initTvInstruments tv ls = do
  treeViewSetModel tv (Just ls)

  treeViewSetHeadersVisible tv True

  -- add a couple columns
  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew
  col3 <- treeViewColumnNew
  col4 <- treeViewColumnNew
  -- col5 <- treeViewColumnNew

  treeViewColumnSetTitle col1 ("Name" :: Text)
  treeViewColumnSetTitle col2 ("Group" :: Text)
  treeViewColumnSetTitle col3 ("File" :: Text)
  treeViewColumnSetTitle col4 ("Directed Chokes" :: Text)
  -- treeViewColumnSetTitle col5 ("Edit Chokes" :: Text)

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew
  renderer3 <- cellRendererTextNew
  renderer4 <- cellRendererToggleNew
  -- renderer5 <- cellRendererPixbufNew

  set renderer2 [cellTextEditable := True, cellTextEditableSet := True]
  set renderer4 [cellXAlign := 0]

  cellLayoutPackStart col1 renderer1 True
  cellLayoutPackStart col2 renderer2 True
  cellLayoutPackStart col3 renderer3 True
  cellLayoutPackStart col4 renderer4 True
  -- cellLayoutPackStart col5 renderer5 True

  cellLayoutSetAttributes col1 renderer1 ls $ \cm ->
    [ cellText := cmName cm
    , cellTextBackgroundColor := yellow
    , cellTextBackgroundSet := cmContainsUndefined cm
    ]
  cellLayoutSetAttributes col2 renderer2 ls $ \cm ->
    [ cellText := fromMaybe "--" (cmGroup cm)
    , cellTextBackgroundColor := yellow
    , cellTextBackgroundSet := cmContainsUndefined cm
    ]
  cellLayoutSetAttributes col3 renderer3 ls $ \cm ->
    [ cellText := cmFile cm
    , cellTextBackgroundColor := yellow
    , cellTextBackgroundSet := cmContainsUndefined cm
    ]
  cellLayoutSetAttributes col4 renderer4 ls $ \cm ->
    [ cellToggleActive := isEnabled (cmChokes cm)
    , cellTextBackgroundColor := yellow
    , cellTextBackgroundSet := cmContainsUndefined cm
    ]
  -- cellLayoutSetAttributes col5 renderer5 ls
  --   $ const [cellPixbufStockId := ("gtk-edit" :: Text)
  --     , cellMode := CellRendererModeActivatable
  --     ]

  _ <- treeViewAppendColumn tv col1
  _ <- treeViewAppendColumn tv col2
  _ <- treeViewAppendColumn tv col3
  _ <- treeViewAppendColumn tv col4

  treeViewSetEnableSearch tv True
  treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
    (i : _) <- treeModelGetPath ls iter
    row     <- listStoreGetValue ls i
    return $ T.toLower str `T.isPrefixOf` T.toLower (cmName row)

  void $ on renderer4 cellToggled $ \str -> do
    let (i : _) = stringToTreePath str
    val <- listStoreGetValue ls i
    listStoreSetValue ls i (val { cmChokes = toggleEnabled (cmChokes val) })

  return renderer2

setInstruments :: DrumkitPage -> [ChannelMap] -> IO ()
setInstruments gui = setListStoreTo (guiTvInstrumentsModel gui)


initTvChannelMap
  :: TreeView
  -> ListStore Text
  -> ListStore ChannelMapItem
  -> ListStore ChannelMap
  -> IORef (Maybe (Int, ChannelMap))
  -> IO CellRendererCombo
initTvChannelMap tv lsMicros ls lsinst ref = do
  treeViewSetModel tv (Just ls)

  treeViewSetHeadersVisible tv True

  -- add a couple columns
  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew
  col3 <- treeViewColumnNew

  treeViewColumnSetTitle col1 ("In" :: Text)
  treeViewColumnSetTitle col2 ("Out" :: Text)
  treeViewColumnSetTitle col3 ("Main" :: Text)

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererComboNew
  renderer3 <- cellRendererToggleNew

  set renderer3 [cellXAlign := 0]

  void $ on renderer3 cellToggled $ \str -> do
    let (i : _) = stringToTreePath str
    val <- listStoreGetValue ls i
    let ena = not (cmiMain val)
    listStoreSetValue ls i (val { cmiMain = ena })

    cont <- readIORef ref
    case cont of
      Just (idx, _) -> do
        cmap <- listStoreGetValue lsinst idx
        let !newCmap = channelMapUpdateMain cmap i ena
        listStoreSetValue lsinst idx newCmap
      Nothing -> return ()


  let colId :: ColumnId Text Text
      colId = makeColumnIdString 0
      -- in this case we take the outputs liststore

  outChans <- listStoreToList lsMicros
  lsChans  <- listStoreNew outChans

  treeModelSetColumn lsChans colId id

  cellLayoutPackStart col1 renderer1 True
  cellLayoutPackStart col2 renderer2 True
  cellLayoutPackStart col3 renderer3 True

  set
    renderer2
    [ cellTextEditable := True
    , cellTextEditableSet := True
    , cellTextBackgroundColor := paleYellow
    , cellTextBackgroundSet := True
    , cellComboTextModel := (lsChans, colId)
    ]

  cellLayoutSetAttributes col1 renderer1 ls $ \i -> [cellText := cmiIn i]
  cellLayoutSetAttributes col2 renderer2 ls
    $ \i -> [cellText := cmiOut i, cellComboTextModel := (lsChans, colId)]
  cellLayoutSetAttributes col3 renderer3 ls
    $ \i -> [cellToggleActive := cmiMain i]

  _ <- treeViewAppendColumn tv col1
  _ <- treeViewAppendColumn tv col2
  _ <- treeViewAppendColumn tv col3

  treeViewSetEnableSearch tv True
  treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
    (i : _)              <- treeModelGetPath ls iter
    ChannelMapItem x _ _ <- listStoreGetValue ls i
    return $ T.toLower str `T.isPrefixOf` T.toLower x

  return renderer2



setChannelMap :: DrumkitPage -> Int -> ChannelMap -> IO ()
setChannelMap gui idx cmap = do
  writeIORef (guiSelectedChannelMap gui) (Just (idx, cmap))
  setListStoreToVec (guiTvChannelMapModel gui) (cmMap cmap)



setupCallbacks :: DrumkitPage -> IO ()
setupCallbacks gui = do
  let instView      = guiTvInstruments gui
      instViewModel = guiTvInstrumentsModel gui

  void $ G.on instView rowActivated $ \(i : _) _ -> do
    !row <- listStoreGetValue instViewModel i
    setChannelMap gui i row
    widgetSetSensitive (guiButtonEditChokes gui) True
    return ()

  void $ G.on (guiButtonEditChokes gui) buttonActivated $ do
    availableInstruments <- map cmName <$> listStoreToList instViewModel
    sel                  <- treeViewGetSelection instView
    rows                 <- treeSelectionGetSelectedRows sel
    case rows of
      ((x : _) : _) -> do
        inst    <- listStoreGetValue instViewModel x
        newInst <- showChokeDialog (guiDirectedChokeDialog gui)
                                   inst
                                   availableInstruments
        forM_ newInst $ listStoreSetValue instViewModel x
      _ -> return ()

  void $ G.on (guiButtonEditClickyKit gui) buttonActivated $ do 
    availableInstruments <- map cmName <$> listStoreToList instViewModel
    kit' <- readIORef (guiDrumkit gui)
    case kit' of 
      Nothing -> return ()
      Just kit -> do 
        case dkInfo kit of 
          Left _ -> return () 
          Right meta -> do 
              let img = case metaImage meta of 
                          Just image -> image 
                          Nothing -> newImageData availableInstruments
              basepath <- entryGetText (guiBaseDir gui)
              newImg' <- showClickyKitDialog (guiClickyKitDialog gui) img basepath
              case newImg' of 
                Nothing -> return () 
                Just newImg -> do 
                  let !newMeta = meta { metaImage = Just newImg }
                      !newDk = kit { dkInfo = Right newMeta }
                  writeIORef (guiDrumkit gui) (Just newDk)

  void $ G.on (guiClickyKitCheck gui) toggled $ do 
    val <- toggleButtonGetActive (guiClickyKitCheck gui)
    widgetSetSensitive (guiButtonEditClickyKit gui) val 

  void $ G.on (guiDkName gui) entryActivated $ do
    nm <- getDkName gui
    when (T.any isSpace nm) $ do
      displayErrorBox (guiDkParentWindow gui)
                      "Drumkit name is not allowed to contain spaces"
      return ()

  -- right click on the channel view shows the popup for add/remove
  void $ G.on (guiTvChannels gui) buttonPressEvent $ do
    bt <- eventButton
    case bt of
      RightButton -> do
        liftIO $ menuPopup (guiChannelMenu gui) Nothing
        return True
      _ -> return False

  -- edit call back for editing the channels
  void $ G.on (guiChannelRenderer gui) edited $ \[i] str -> do
    oldVal <- listStoreGetValue (guiTvChannelsModel gui) i
    -- set the GTK list store to the new value
    listStoreSetValue (guiTvChannelsModel gui) i str
    -- now loop over every channel map and update it with the new microphone
    mapInsts gui (cmChangeChannel oldVal str)
    -- clear the channel map view, so the user has to reactivate it
    listStoreClear (guiTvChannelMapModel gui)

  -- callback for setting up the combo box renderer used for setting the out channel
  -- Unfortunately this is necessary for GTK3 as it otherwise doesn't work
  void $ G.on (guiOutChannelRenderer gui) editingStarted $ \widget treepath ->
    do
      case treepath of
        [_] -> do
          comboListStore <- comboBoxSetModelText (castToComboBox widget)
          outChans       <- listStoreToList (guiTvChannelsModel gui)
          mapM_ (listStoreAppend comboListStore) (outChans :: [Text])
        _ -> return ()

  -- callback for editing the channel
  void $ G.on (guiOutChannelRenderer gui) edited $ \[i] str -> do
    ChannelMapItem inc _ mn <- listStoreGetValue (guiTvChannelMapModel gui) i
    -- set the GTK list store to the new value
    let val' = ChannelMapItem inc str mn
    listStoreSetValue (guiTvChannelMapModel gui) i val'
    -- we also need to set the new value in the instrument itself

    sel  <- treeViewGetSelection (guiTvInstruments gui)
    path <- treeSelectionGetSelectedRows sel
    case path of
      ((idx : _) : _) -> do
        hsVal <- listStoreGetValue (guiTvInstrumentsModel gui) idx
        let cm  = V.toList $ cmMap hsVal
            cm' = zipWith upd cm [0 ..]
            upd s j = if j == i then val' else s
            hsVal' = hsVal { cmMap = V.fromList cm' }
        listStoreSetValue (guiTvInstrumentsModel gui) idx hsVal'
      _ -> return ()

  -- edit call back for editing the channels
  void $ G.on (guiGroupRenderer gui) edited $ \[i] str -> do
    oldVal <- listStoreGetValue (guiTvInstrumentsModel gui) i
    -- set the GTK list store to the new value
    let newVal = if str == "" || str == "--"
          then oldVal { cmGroup = Nothing }
          else oldVal { cmGroup = Just str }
    listStoreSetValue (guiTvInstrumentsModel gui) i newVal

  -- right click on the channel view shows the popup for add/remove
  void $ G.on (guiTvChannelMap gui) buttonPressEvent $ do
    bt <- eventButton
    case bt of
      RightButton -> do
        liftIO $ menuPopup (guiChannelMapMenu gui) Nothing
        return True
      _ -> return False


mapInsts :: DrumkitPage -> (ChannelMap -> ChannelMap) -> IO ()
mapInsts gui f = do
  insts <- listStoreToList (guiTvInstrumentsModel gui)
  let newInsts = map (cmUpdateIfUndefined . f) insts
  setListStoreTo (guiTvInstrumentsModel gui) newInsts




exportDrumKit :: DrumkitPage -> IO ()
exportDrumKit gui = do
  basepath <- entryGetText (guiBaseDir gui)
  if null basepath
    then displayErrorBox (guiDkParentWindow gui) "No basepath specified!"
    else do
      nm' <- getName gui
      case nm' of 
        Nothing -> do         
          displayErrorBox (guiDkParentWindow gui)
                             "No drumkit name specified!"
        Just nm -> withFileHandlingDialog (guiFhDialog gui) $ do
          writeDrumKitFile gui nm basepath



writeDrumKitFile :: DrumkitPage -> Text -> FilePath -> IO ()
writeDrumKitFile gui nm basepath = do
  catch
    (writeDrumKitFile' gui nm basepath)
    (\e -> displayErrorBox
      (guiDkParentWindow gui)
      ("Error during export: " <> T.pack (show (e :: SomeException)))
    )


writeDrumKitFile' :: DrumkitPage -> Text -> FilePath -> IO ()
writeDrumKitFile' gui nm basepath = do
  dir <- createDrumgizmoDirectories basepath
  case dir of
    Left err ->
      displayErrorBox (guiDkParentWindow gui) ("Error during export: " <> err)
    Right () -> do
      drumkit <- getDrumkitGUI gui
      basedir <- entryGetText (guiBaseDir gui)
      let dgPath       = getDrumgizmoDir basedir
          drumkitFName = dgPath </> T.unpack nm <.> ".xml"

      setDrumkit gui drumkit

      res <-
        askUserForOverwriteIfNecessary (guiFhDialog gui) drumkitFName
          $ writeDrumKitXML drumkit drumkitFName
      case res of
        Left  err -> displayErrorBox (guiDkParentWindow gui) err
        Right _   -> do
            -- also export the instrument files
          exportInstruments gui


getDrumkitGUI :: DrumkitPage -> IO Drumkit
getDrumkitGUI gui = do
  drumkit  <- getDrumkit gui
  channels <- listStoreToList (guiTvChannelsModel gui)
  insts    <- listStoreToList (guiTvInstrumentsModel gui)
  hasMeta  <- toggleButtonGetActive (guiMetaEnable gui)
  case drumkit of
    Just d -> do
      if hasMeta
        then do
          meta <- getDkMetaData gui
          let
            d' = d { dkInfo        = Right meta
                   , dkChannels    = channels
                   , dkInstruments = insts
                   }
          return d'
        else do
          nm   <- getDkName gui
          desc <- getDkDescription gui
          let
            oldDesc = OldDescr nm desc
            d'      = d { dkInfo        = Left oldDesc
                        , dkChannels    = channels
                        , dkInstruments = insts
                        }
          return d'
    Nothing -> do
      if hasMeta
        then do
          meta <- getDkMetaData gui
          sr   <- getDkSampleRate gui
          let d = Drumkit { dkInfo        = Right meta
                          , dkSampleRate  = (Just (T.pack (show sr)))
                          , dkChannels    = channels
                          , dkInstruments = insts
                          }
          return d
        else do
          nm   <- getDkName gui
          desc <- getDkDescription gui
          sr   <- getDkSampleRate gui
          let oldDesc = OldDescr nm desc
              d       = Drumkit { dkInfo        = Left oldDesc
                                , dkSampleRate  = (Just (T.pack (show sr)))
                                , dkChannels    = channels
                                , dkInstruments = insts
                                }
          return d


saveDrumkit :: DrumkitPage -> IO ()
saveDrumkit gui = do
  basepath <- entryGetText (guiBaseDir gui)
  nm       <- T.unpack <$> getDkName gui

  let parentWindow = guiDkParentWindow gui

  dialog <- fileChooserDialogNew
    (Just ("Save Drumkit File" :: Text))             --dialog title
    (Just parentWindow)                     --the parent window
    FileChooserActionSave                         --the kind of dialog we want
    [ ( "gtk-cancel"                                --The buttons to display
      , ResponseCancel
      )
    , ("gtk-save", ResponseAccept)
    ]

  void $ fileChooserSetCurrentFolder dialog (getDrumgizmoDir basepath)
  void $ fileChooserSetCurrentName dialog nm


  widgetShow dialog
  resp <- dialogRun dialog
  case resp of
    ResponseAccept -> do
      nam <- fileChooserGetFilename dialog
      bp  <- fileChooserGetCurrentFolder dialog

      case (nam, bp) of
        (Just name, Just dir) -> do
          withFileHandlingDialog (guiFhDialog gui) $ do
            drumkit <- getDrumkitGUI gui
            let drumkitFName' = dir </> name
                drumkitFName  = if takeExtension drumkitFName' == ".xml"
                  then drumkitFName'
                  else addExtension drumkitFName' ".xml"
            res <-
              askUserForOverwriteIfNecessary (guiFhDialog gui) drumkitFName
                $ writeDrumKitXML drumkit drumkitFName
            case res of
              Left  err -> displayErrorBox (guiDkParentWindow gui) err
              Right _   -> return ()
        _ -> return ()
    _ -> return ()
  widgetHide dialog



exportInstruments :: DrumkitPage -> IO ()
exportInstruments gui = do
  v    <- readIORef (guiDkInstrumentPages gui)
  vres <- V.forM v instrumentPageWriteInstrumentFile

  if V.any isLeft vres
    then do
      let errs' = V.filter isLeft vres
          errs  = lefts $ V.toList errs'
      displayMultiErrors (guiErrDiag gui)
                         "Multiple Errors during export of Instrument Files:"
                         errs
    else do
      displayInfoBox (guiDkParentWindow gui) "Successfully exported drumkit."



resetDrumkit :: DrumkitPage -> IO ()
resetDrumkit gui = do
  listStoreClear (guiTvChannelsModel gui)
  listStoreClear (guiTvInstrumentsModel gui)
  listStoreClear (guiTvChannelMapModel gui)

  clearDrumkit gui

  v <- readIORef (guiDkInstrumentPages gui)
  V.mapM_ instrumentPageReset v
  writeIORef (guiDkInstrumentPages gui) V.empty

  resetMidiMap (guiMidiMapGM gui)
  resetMidiMap (guiMidiMapDef gui)

  return ()


getInfo :: DrumkitPage -> IO (Either OldDescr MetaData)
getInfo gui = do
  ena <- toggleButtonGetActive (guiMetaEnable gui)
  if ena
    then Right <$> getDkMetaData gui
    else do
      nm   <- getDkName gui
      desc <- getDkDescription gui
      return $ Left (OldDescr nm desc)


compileDrumkit :: DrumkitPage -> IO ()
compileDrumkit gui = do
  inst  <- readIORef (guiDkInstrumentPages gui)

  instF <- V.mapM instrumentPageGetInstrumentFile inst

  let errs = lefts (V.toList instF)
  if not (null errs)
    then do
      displayMultiErrors
        (guiErrDiag gui)
        "Multiple errors happened during Import of Instruments:"
        errs
      return ()
    else do
      info <- getInfo gui
      sr   <- getDkSampleRateText gui

      let drumkit = generateDrumkit info (Just sr) insts
          insts   = rights (V.toList instF)

      -- set the actual drumkit
      setDrumkit gui drumkit

      -- set the channels for viewing
      setChannels gui (dkChannels drumkit)
      setInstruments gui (dkInstruments drumkit)

      -- also convert the drumkit to a midi map
      let midimap = getMidiMap drumkit

      setMidiMap (guiMidiMapGM gui)  midimap
      setMidiMap (guiMidiMapDef gui) midimap


addChannel :: DrumkitPage -> IO ()
addChannel gui = do
  let def = T.pack (showMic Undefined)
  idx <- listStoreAppend (guiTvChannelsModel gui) def
  treeViewSetCursor (guiTvChannels gui) [idx] Nothing
  activateRow (guiTvChannels gui) idx


removeChannel :: DrumkitPage -> IO ()
removeChannel gui = do
  sel  <- treeViewGetSelection (guiTvChannels gui)
  path <- treeSelectionGetSelectedRows sel
  case path of
    ((idx : _) : _) -> do
      chan <- listStoreGetValue (guiTvChannelsModel gui) idx
      listStoreRemove (guiTvChannelsModel gui) idx
      let def = T.pack (showMic Undefined)
          val = chan
      -- now loop over every channel map and update it with the new microphone
      mapInsts gui (cmChangeChannel val def)
      -- clear the channel map view, so the user has to reactivate it
      listStoreClear (guiTvChannelMapModel gui)

      -- if we have removed the Undefined and we still have some
      -- undefined channels, we need to add it again
      addUndefinedIfNeeded gui

    _ -> return ()


addUndefinedIfNeeded :: DrumkitPage -> IO ()
addUndefinedIfNeeded gui = do
  insts <- listStoreToList (guiTvInstrumentsModel gui)
  let undef = filter cmAnyUndefined insts
  unless (null undef) $ do
    ls <- listStoreToList (guiTvChannelsModel gui)
    let def = T.pack (showMic Undefined)
    case L.find (== def) ls of
      Nothing -> addChannel gui  -- if we don't find Undefined and we need it
                                 -- because there are undefined channel mappings
                                 -- we have to re-add it
      Just _  -> return ()



loadDrumkit :: DrumkitPage -> IO ()
loadDrumkit gui = do
  let parentWindow = guiDkParentWindow gui
  dialog <- fileChooserDialogNew
    (Just ("Load a Drumkit" :: Text))             --dialog title
    (Just parentWindow)                     --the parent window
    FileChooserActionOpen                         --the kind of dialog we want
    [ ( "gtk-cancel"                                --The buttons to display
      , ResponseCancel
      )
    , ("gtk-open", ResponseAccept)
    ]

  widgetShow dialog
  resp <- dialogRun dialog
  case resp of
    ResponseAccept -> do
      f <- fileChooserGetFilename dialog
      case f of
        Nothing   -> return ()
        Just file -> do
          loadDrumkit' gui file
          return ()
    _ -> return ()
  widgetHide dialog


loadDrumkit' :: DrumkitPage -> FilePath -> IO ()
loadDrumkit' gui file = do
  res <- importDrumkitFile file
  case res of
    Left err -> displayErrorBox (guiDkParentWindow gui)
                                ("Error on loading drumkit: " <> err)
    Right dk -> do
        --putStrLn $ "Imported Drumkit:\n" <> show dk

      resetDrumkit gui

      let basepath = getBasePath file
          basepathT :: Text
          basepathT = T.pack basepath
      entrySetText (guiBaseDir gui)    basepathT
      entrySetText (guiSamplesDir gui) basepathT

      showDrumkit gui dk

      -- now load the instrument files
      loadInstrumentFiles gui (takeDirectory file) (dkInstruments dk)
      return ()

showDrumkit :: DrumkitPage -> Drumkit -> IO ()
showDrumkit gui dk = do
  case dkSampleRate dk of
    Just sr -> setDkSampleRateText gui sr
    Nothing -> return ()
  case dkInfo dk of
    Left descr -> do
      toggleButtonSetActive (guiMetaEnable gui) False
      entrySetText (guiDkName gui) (odName descr)
      setDkDescription gui (odDescription descr)
    Right meta -> do
      toggleButtonSetActive (guiMetaEnable gui) False
      setDkMetaData gui (Just meta)

  -- set the actual drumkit
  setDrumkit gui dk

  -- set the channels for viewing
  setChannels gui (dkChannels dk)
  setInstruments gui (dkInstruments dk)



loadInstrumentFiles :: DrumkitPage -> FilePath -> [ChannelMap] -> IO ()
loadInstrumentFiles gui path files = do
  mapM_ loadFile files
 where
  loadFile cm = do
    let name = cmName cm
    ins <- instrumentPageNew (guiDkParentWindow gui)
                             (guiDkInstrumentsNotebook gui)
                             (guiBaseDir gui)
                             (guiSamplesDir gui)
                             (guiParserCombo gui)
                             (guiDkInstrumentPages gui)
                             (guiFhDialog gui)
                             (guiErrDiag gui)
                             (setDkSampleRate gui)
    void $ notebookAppendPage (guiDkInstrumentsNotebook gui)
                              (instrumentPageGetMainBox ins)
                              name
    instrumentPageInsert ins
    instrumentPageSetInstrumentName ins name
    instrumentPageLoadFile ins (path </> cmFile cm)


-- Can't do it this way, as the channel mapping must be unique. What we got this
-- way was, that even if in the channel mapping was (Close -> L and Close -> R)
-- only the right channel was played as Close is ambigous.

-- Solution is to convert the instruments themselves to stereo and perform a new
-- mapping

{-convertToFullMix :: DrumkitPage -> IO ()
convertToFullMix gui = do
    -- got through all instruments an change the channel mapping to Full Mix
    mapInsts gui convert

    -- remove all channels and add the FullMix channels
    setListStoreTo (guiTvChannelsModel gui) (map (pack.showMic) [FullMixL, FullMixR])

    listStoreClear (guiTvChannelMapModel gui)

    return ()
    where
        convert :: ChannelMap -> ChannelMap
        convert x = x { cmMap = func (cmMap x) }
        func :: [(Text, Text)] -> [(Text, Text)]
        func [] = []
        func ((inc, outc) : xs) | isLeftChannel outc = (inc, pack (showMic FullMixL)) : func xs
                                 | isRightChannel outc = (inc, pack (showMic FullMixR)) : func xs
                                 | otherwise = (inc, pack (showMic FullMixL)) : (inc, pack (showMic FullMixR)) : func xs-}

convertToFullMix :: DrumkitPage -> IO ()
convertToFullMix gui = do
    -- first create the new stereo channels by mapping over all instrument pages
  v <- readIORef (guiDkInstrumentPages gui)
  V.mapM_ instrumentPageConvertToFullMix v
  -- then compile a new drumkit
  compileDrumkit gui
  -- got through all instruments an change the channel mapping to Full Mix
  mapInsts gui convert

  -- remove all channels and add the FullMix channels
  setListStoreTo (guiTvChannelsModel gui)
                 (map (T.pack . showMic) [FullMixL, FullMixR])

  listStoreClear (guiTvChannelMapModel gui)

  return ()
 where
  convert :: ChannelMap -> ChannelMap
  convert x = x { cmMap = func (cmMap x) }
  func = V.fromList . func' . V.toList
  func' :: [ChannelMapItem] -> [ChannelMapItem]
  func' [] = []
  func' (ChannelMapItem inc outc mn : xs)
    | isLeftChannel outc
    = ChannelMapItem inc (T.pack (showMic FullMixL)) mn : func' xs
    | isRightChannel outc
    = ChannelMapItem inc (T.pack (showMic FullMixR)) mn : func' xs
    | otherwise
    = ChannelMapItem inc (T.pack (showMic FullMixL)) mn
      : ChannelMapItem inc (T.pack (showMic FullMixR)) mn
      : func' xs




duplicateCM :: DrumkitPage -> IO ()
duplicateCM gui = do
    -- get seledted cm and duplicate it
  sel <- treeViewGetSelection (guiTvChannelMap gui)
  s   <- treeSelectionGetSelectedRows sel
  case s of
    ((x : _) : _) -> do
      val <- listStoreGetValue (guiTvChannelMapModel gui) x
      listStoreInsert (guiTvChannelMapModel gui) (x + 1) val

      sel1 <- treeViewGetSelection (guiTvInstruments gui)
      s1   <- treeSelectionGetSelectedRows sel1
      case s1 of
        ((i : _) : _) -> do
          cm   <- listStoreGetValue (guiTvInstrumentsModel gui) i
          vals <- listStoreToList (guiTvChannelMapModel gui)
          listStoreSetValue (guiTvInstrumentsModel gui)
                            i
                            (cm { cmMap = V.fromList vals })
        _ -> return ()

    _ -> return ()


removeCM :: DrumkitPage -> IO ()
removeCM gui = do
  sel <- treeViewGetSelection (guiTvChannelMap gui)
  s   <- treeSelectionGetSelectedRows sel
  case s of
    ((x : _) : _) -> do
      listStoreRemove (guiTvChannelMapModel gui) x

      sel1 <- treeViewGetSelection (guiTvInstruments gui)
      s1   <- treeSelectionGetSelectedRows sel1
      case s1 of
        ((i : _) : _) -> do
          cm   <- listStoreGetValue (guiTvInstrumentsModel gui) i
          vals <- listStoreToList (guiTvChannelMapModel gui)
          listStoreSetValue (guiTvInstrumentsModel gui)
                            i
                            (cm { cmMap = V.fromList vals })
        _ -> return ()
    _ -> return ()


compileMidiMapGM :: DrumkitPage -> IO ()
compileMidiMapGM gui = do
    -- also convert the drumkit to a midi map
  dr <- getDrumkit gui
  case dr of
    Nothing      -> return ()
    Just drumkit -> do
      let midimap = getMidiMap drumkit
      setMidiMap (guiMidiMapGM gui) midimap


compileMidiMapDefault :: DrumkitPage -> IO ()
compileMidiMapDefault gui = do
    -- also convert the drumkit to a midi map
  dr <- getDrumkit gui
  case dr of
    Nothing      -> return ()
    Just drumkit -> do
      let midimap = getMidiMap drumkit
      setMidiMap (guiMidiMapDef gui) midimap


channelUp :: DrumkitPage -> IO ()
channelUp gui = do
  sel1 <- treeViewGetSelection (guiTvChannels gui)
  s1   <- treeSelectionGetSelectedRows sel1
  case s1 of
    ((i : _) : _) -> do
      let ls = guiTvChannelsModel gui
      ch <- listStoreGetValue ls i
      listStoreRemove ls i
      let idx = if i > 0 then i - 1 else 0
      listStoreInsert ls idx ch
      activateRow (guiTvChannels gui) idx
    _ -> return ()


channelDown :: DrumkitPage -> IO ()
channelDown gui = do
  sel1 <- treeViewGetSelection (guiTvChannels gui)
  s1   <- treeSelectionGetSelectedRows sel1
  case s1 of
    ((i : _) : _) -> do
      let ls = guiTvChannelsModel gui
      ch <- listStoreGetValue ls i
      listStoreRemove ls i
      let idx = i + 1
      listStoreInsert ls idx ch
      activateRow (guiTvChannels gui) idx
    _ -> return ()



sortChannels :: DrumkitPage -> IO ()
sortChannels gui = do
  chans <- listStoreToList (guiTvChannelsModel gui)
  let newChans = map snd . sortOn fst . map chanToOrd $ chans
  setListStoreTo (guiTvChannelsModel gui) newChans
  return ()


chanToOrd :: Text -> (Int, Text)
chanToOrd x =
  let !xx = T.toLower x
      worker | "kick" `T.isInfixOf` xx    = (0, x)
             | "snare" `T.isInfixOf` xx   = (1, x)
             | "hihat" `T.isInfixOf` xx   = (2, x)
             | "tom" `T.isInfixOf` xx     = (3, x)
             | "floor" `T.isInfixOf` xx   = (4, x)
             | "ride" `T.isInfixOf` xx    = (5, x)
             | "ohl" == xx                = (6, x)
             | "ohr" == xx                = (6, x)
             | "room" `T.isInfixOf` xx    = (7, x)
             | "fullmix" `T.isInfixOf` xx = (8, x)
             | otherwise                  = (100, x)
  in  worker
