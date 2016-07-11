{-# LANGUAGE OverloadedStrings, BangPatterns, NoImplicitPrelude #-}
module Gtk.Drumkit

where


import ClassyPrelude

import Prelude (read)

--import Control.Monad (void)
--import Control.Exception

import System.FilePath.Find as F
import System.FilePath

import Data.Types
import Data.DrumDrops.Utils
import Data.Char (isSpace)
import Data.Export
import Data.Drumgizmo
--import Data.Import
import Data.Either

--import qualified Data.ByteString.Lazy as B

import qualified Data.Vector as V

import qualified Data.Text as T
--import Data.IORef
import qualified Data.Set as S

import Graphics.UI.Gtk as G

import Gtk.Utils
import Gtk.InstrumentFrame
import Gtk.ErrorDialog
import Gtk.MidiMap



data DrumkitPage = DrumkitPage {
    guiDkParentWindow :: Window,
    guiDkName :: Entry,
    guiDkDescription :: TextView,
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
    guiTvChannelsModel :: ListStore Microphones,
    guiTvInstrumentsModel :: ListStore ChannelMap,
    guiTvChannelMapModel :: ListStore (Text, Text),
    guiDrumkit :: IORef (Maybe Drumkit),
    guiDkInstrumentPages :: IORef (Vector InstrumentPage),
    guiErrDiag :: ErrorDialog,
    guiMidiMapGM :: MidiMapPage,
    guiMidiMapDef :: MidiMapPage,
    guiParserCombo :: ComboBox
}


initDrumkitPage :: Window ->
                    G.Builder ->
                    Notebook ->
                    ProgressBar ->
                    ComboBox ->
                    Entry ->
                    Entry ->
                    IORef (Vector InstrumentPage) ->
                    IO DrumkitPage
initDrumkitPage mainWindow builder instrumentsNotebook progress combo entryBaseDirectory entrySamplesDir ioref = do

    buttonImportDrumkit <- builderGetObject builder castToButton ("buttonImportDrumkit" :: Text)
    buttonExportDrumkit <- builderGetObject builder castToButton ("buttonExportDrumkit" :: Text)
    buttonSetBaseDir <- builderGetObject builder castToButton ("buttonSetBaseDir" :: Text)
    buttonSetSamplesDir <- builderGetObject builder castToButton ("buttonSetSamplesDir" :: Text)

    tvChannels <- builderGetObject builder castToTreeView ("treeviewChannels" :: Text)
    tvInstruments <- builderGetObject builder castToTreeView ("treeviewInstruments" :: Text)
    tvChannelMap <- builderGetObject builder castToTreeView ("treeviewChannelMap" :: Text)

    eName <- builderGetObject builder castToEntry ("entryDkName" :: Text)
    eDesc <- builderGetObject builder castToTextView ("textviewDkDescription" :: Text)

    tvMidiGM <- builderGetObject builder castToTreeView ("treeviewMidiMapGM" :: Text)
    tvMidiDef <- builderGetObject builder castToTreeView ("treeviewMidiMapDef" :: Text)

    lsm <- listStoreNew []
    lsinst <- listStoreNew []
    lscm <- listStoreNew []

    errDiag <- initErrorDialog builder

    dr <- newIORef Nothing

    initTvChannels tvChannels lsm
    initTvInstruments tvInstruments lsinst
    initTvChannelMap tvChannelMap lscm

    entrySetText entryBaseDirectory ("/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Multi Velocity Pack" :: FilePath)
    entrySetText entrySamplesDir ("/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Multi Velocity Pack/SAMPLES" :: FilePath)

    entrySetText eName ("MapexHeavyRockMultiVelocity" :: Text)

    gmLoadButton <- builderGetObject builder castToButton ("buttonLoadGMMap" :: Text)
    gmExportButton <- builderGetObject builder castToButton ("buttonExportGMMap" :: Text)

    defLoadButton <- builderGetObject builder castToButton ("buttonLoadDefMap" :: Text)
    defExportButton <- builderGetObject builder castToButton ("buttonExportDefMap" :: Text)

    resetButton <- builderGetObject builder castToButton ("buttonReset" :: Text)

    midiMapGm <- initMidiMap mainWindow tvMidiGM entryBaseDirectory gmLoadButton gmExportButton
    midiMapDef <- initMidiMap mainWindow tvMidiDef entryBaseDirectory defLoadButton defExportButton

    initParserCombo combo

    let gui = DrumkitPage{
            guiDkParentWindow = mainWindow,
            guiTvChannels = tvChannels,
            guiTvInstruments = tvInstruments,
            guiTvChannelMap = tvChannelMap,
            guiTvChannelsModel = lsm,
            guiTvInstrumentsModel = lsinst,
            guiTvChannelMapModel = lscm,
            guiDrumkit = dr,
            guiDkName = eName,
            guiDkDescription = eDesc,
            guiBaseDir = entryBaseDirectory,
            guiSamplesDir = entrySamplesDir,
            guiBtImportDrumkit = buttonImportDrumkit,
            guiBtSetBaseDir = buttonSetBaseDir,
            guiBtSetSamplesDir = buttonSetSamplesDir,
            guiDkInstrumentsNotebook = instrumentsNotebook,
            guiDkProgress = progress,
            guiErrDiag = errDiag,
            guiDkInstrumentPages = ioref,
            guiMidiMapGM = midiMapGm,
            guiMidiMapDef = midiMapDef,
            guiParserCombo = combo
        }

    setDkDescription gui "Mapex Heavy Rock Kit patch from the All Samples Pack from DrumDrops (http://www.drumdrops.com). Created by M. Oswald."

    void $ G.on buttonSetBaseDir buttonActivated $ setBaseDir gui
    void $ G.on buttonSetSamplesDir buttonActivated $ setSamplesDir gui

    void $ G.on buttonImportDrumkit buttonActivated $ importDrumDropsDrumKit gui
    void $ G.on buttonExportDrumkit buttonActivated $ exportDrumKit gui

    void $ G.on resetButton buttonActivated $ resetDrumkit gui

    setupCallbacks gui

    return gui


getDkName :: DrumkitPage -> IO Text
getDkName dkp = entryGetText (guiDkName dkp)


getDkDescription :: DrumkitPage -> IO Text
getDkDescription dkp = do
    buffer <- textViewGetBuffer (guiDkDescription dkp)
    (start, end) <- textBufferGetBounds buffer
    res <- textBufferGetText buffer start end False
    return $ filter (/= '\n') res


setDkDescription :: DrumkitPage -> Text -> IO ()
setDkDescription dkp desc = do
    buffer <- textViewGetBuffer (guiDkDescription dkp)
    textBufferSetText buffer desc

initParserCombo :: ComboBox -> IO ()
initParserCombo cb = do
    void $ comboBoxSetModelText cb
    void $ mapM (comboBoxAppendText cb) str
    comboBoxSetActive cb 0
    where
        str = map (pack . show) [MapexParser]

setBaseDir :: DrumkitPage -> IO ()
setBaseDir mainWindow = do
    let parentWindow = guiDkParentWindow mainWindow
    dialog <- fileChooserDialogNew
              (Just $ ("Set Base Directory for Imports" :: Text))             --dialog title
              (Just parentWindow)                     --the parent window
              FileChooserActionSelectFolder                         --the kind of dialog we want
              [("gtk-cancel"                                --The buttons to display
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]

    basepath <- entryGetText (guiBaseDir mainWindow)
    void $ fileChooserSetFilename dialog basepath

    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept -> do
            f <- fileChooserGetFilename dialog
            case f of
                Nothing -> return ()
                Just dir -> do
                    entrySetText (guiBaseDir mainWindow) dir
                    txt <- entryGetText (guiSamplesDir mainWindow)
                    if null (txt :: Text) then entrySetText (guiSamplesDir mainWindow) dir else return ()
                    return ()
        ResponseCancel -> return ()
        ResponseDeleteEvent -> return ()
        _ -> return ()
    widgetHide dialog


setSamplesDir :: DrumkitPage -> IO ()
setSamplesDir mainWindow = do
    let parentWindow = guiDkParentWindow mainWindow
    dialog <- fileChooserDialogNew
              (Just $ ("Set Sample Base Directory for Imports" :: Text))             --dialog title
              (Just parentWindow)                     --the parent window
              FileChooserActionSelectFolder                         --the kind of dialog we want
              [("gtk-cancel"                                --The buttons to display
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]
    loc <- entryGetText (guiSamplesDir mainWindow)
    void $ fileChooserSetFilename dialog loc

    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept -> do
            f <- fileChooserGetFilename dialog
            case f of
                Nothing -> return ()
                Just dir -> do
                    entrySetText (guiSamplesDir mainWindow) dir
                    return ()
        ResponseCancel -> return ()
        ResponseDeleteEvent -> return ()
        _ -> return ()
    widgetHide dialog





importDrumDropsDrumKit :: DrumkitPage -> IO ()
importDrumDropsDrumKit gui = do
    widgetSetSensitive (guiParserCombo gui) False
    catch (importDrumDropsDrumKit' gui)
        (\e -> displayErrorBox (guiDkParentWindow gui) ("Error: " <> pack (show (e :: SomeException))))
    widgetSetSensitive (guiParserCombo gui) True



importDrumDropsDrumKit' :: DrumkitPage -> IO ()
importDrumDropsDrumKit' gui = do
    b <- entryGetText (guiBaseDir gui) :: IO FilePath
    case b of
        "" -> displayErrorBox (guiDkParentWindow gui) "Basedir must be set!"
        basedir -> do
            samplesDir <- entryGetText (guiSamplesDir gui)

            dirs <- getDirectoriesToImport samplesDir

            -- first clear the instruments notebook
            clearNotebook (guiDkInstrumentsNotebook gui)

            instFiles <- doImport basedir samplesDir dirs

            let errs = lefts instFiles
            case null errs of
                False -> do
                    displayMultiErrors (guiErrDiag gui) "Multiple errors happened during Import of Instruments:" errs
                    return ()
                True -> do
                    let insts = rights instFiles
                    nm <- getDkName gui
                    desc <- getDkDescription gui

                    let drumkit = generateDrumkit nm desc insts

                    -- set the actual drumkit
                    writeIORef (guiDrumkit gui) (Just drumkit)

                    -- set the channels for viewing
                    setChannels gui (dkChannels drumkit)
                    setInstruments gui (dkInstruments drumkit)

                    -- also convert the drumkit to a midi map
                    let midimap = getMidiMap drumkit

                    setMidiMap (guiMidiMapGM gui) midimap
                    setMidiMap (guiMidiMapDef gui) midimap

                    return ()
            return ()
    where
        doImport :: FilePath -> FilePath -> [FilePath] -> IO [Either Text InstrumentFile]
        doImport basedir samplesDir paths = do
            -- import the instruments
            let progress = guiDkProgress gui
                n = length paths
                step :: Double
                step = 1.0 / fromIntegral n
            progressBarSetText progress ("Importing DrumDrops Drumkit..." :: Text)

            instruments <- forM paths (doSingleImport progress basedir samplesDir step)

            progressBarSetText progress ("" :: Text)
            progressBarSetFraction progress 0.0

            return instruments

        doSingleImport progress basedir samplesDir step path = do
            ins <- newInstrumentPage (guiDkParentWindow gui) (guiDkInstrumentsNotebook gui)
                (guiBaseDir gui) (guiSamplesDir gui) (guiParserCombo gui) (guiDkInstrumentPages gui)
            let instName = pathToInstrument samplesDir path
            _ <- notebookAppendPage (guiDkInstrumentsNotebook gui) (getMainBox ins) instName
            insertInstrumentPage ins

            pt <- comboBoxGetActiveText (guiParserCombo gui)
            let parserType = maybe MapexParser (read . unpack) pt

            res <- importInstrument parserType basedir samplesDir path
            case res of
                Left err -> do
                    displayErrorBox (guiDkParentWindow gui) err
                    return (Left err)
                Right instFile -> do
                    setInstrumentFile ins instFile

                    -- update the progress bar
                    frac <- progressBarGetFraction progress
                    progressBarSetFraction progress (frac + step)

                    yield

                    return (Right instFile)
        yield = do
            i <- eventsPending
            when (i > 0) $ do
                void $ mainIteration
                yield




getDirectoriesToImport :: FilePath -> IO [FilePath]
getDirectoriesToImport path = do
    dirs <- F.find recP filterP path
    let s :: Set FilePath
        s = S.fromList $ map takeDirectory dirs
    return (S.toList s)
    where
        recP = always
        filterP = extension ==? ".wav"


initTvChannels :: TreeView -> ListStore Microphones -> IO ()
initTvChannels tv ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Channels" :: Text)

    renderer1 <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True

    cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [ cellText := T.pack (show hs)]

    _ <- treeViewAppendColumn tv col1

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `isPrefixOf` toLower (show row)

    return ()


setChannels :: DrumkitPage -> [Microphones] -> IO ()
setChannels gui channels = setListStoreTo (guiTvChannelsModel gui) channels


initTvInstruments :: TreeView -> ListStore ChannelMap -> IO ()
initTvInstruments tv ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew
    col3 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Name" :: Text)
    treeViewColumnSetTitle col2 ("Group" :: Text)
    treeViewColumnSetTitle col3 ("File" :: Text)

    renderer1 <- cellRendererTextNew
    renderer2 <- cellRendererTextNew
    renderer3 <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True
    cellLayoutPackStart col3 renderer3 True

    cellLayoutSetAttributes col1 renderer1 ls $ \cm -> [ cellText := cmName cm]
    cellLayoutSetAttributes col2 renderer2 ls $ \cm -> [ cellText := maybe "--" id (cmGroup cm)]
    cellLayoutSetAttributes col3 renderer3 ls $ \cm -> [ cellText := cmFile cm]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2
    _ <- treeViewAppendColumn tv col3

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `isPrefixOf` toLower (cmName row)

    return ()

setInstruments :: DrumkitPage -> [ChannelMap] -> IO ()
setInstruments gui insts = setListStoreTo (guiTvInstrumentsModel gui) insts


initTvChannelMap :: TreeView -> ListStore (Text, Text) -> IO ()
initTvChannelMap tv ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("In" :: Text)
    treeViewColumnSetTitle col2 ("Out" :: Text)

    renderer1 <- cellRendererTextNew
    renderer2 <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True

    cellLayoutSetAttributes col1 renderer1 ls $ \(i, _) -> [ cellText := i]
    cellLayoutSetAttributes col2 renderer2 ls $ \(_, o) -> [ cellText := o]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        (x, _) <- listStoreGetValue ls i
        return $ toLower str `isPrefixOf` toLower x

    return ()



setChannelMap :: DrumkitPage -> [(Text, Text)] -> IO ()
setChannelMap gui cmap = setListStoreTo (guiTvChannelMapModel gui) cmap



setupCallbacks :: DrumkitPage -> IO ()
setupCallbacks gui = do
    let instView = guiTvInstruments gui
        instViewModel = guiTvInstrumentsModel gui

    void $ G.on instView rowActivated $ \(i:_) _ -> do
        !row <- listStoreGetValue instViewModel i
        setChannelMap gui (cmMap row)

        return ()

    void $ G.on (guiDkName gui) entryActivated $ do
        nm <- getDkName gui
        if (any isSpace nm)
            then do
                displayErrorBox (guiDkParentWindow gui) "Drumkit name is not allowed to contain spaces"
                return ()
            else return ()


exportDrumKit :: DrumkitPage -> IO ()
exportDrumKit gui = do
    basepath <- entryGetText (guiBaseDir gui)
    case null basepath of
        True -> displayErrorBox (guiDkParentWindow gui) "No basepath specified!"
        False -> do
            nm <- getDkName gui
            case null nm of
                True -> displayErrorBox (guiDkParentWindow gui) "No drumkit name specified!"
                False -> do
                    writeDrumKitFile gui nm basepath
                    -- get the midi map and write it
                    gmMidi <- getMidiMapFromGUI (guiMidiMapGM gui)
                    defMidi <- getMidiMapFromGUI (guiMidiMapDef gui)

                    writeMidiMapFile (guiMidiMapGM gui) "MIDIMap_GM.xml" gmMidi
                    writeMidiMapFile (guiMidiMapDef gui) "MIDIMap_Default.xml" defMidi



writeDrumKitFile :: DrumkitPage -> Text -> FilePath -> IO ()
writeDrumKitFile gui nm basepath = do
    catch (writeDrumKitFile' gui nm basepath)
        (\e -> displayErrorBox (guiDkParentWindow gui) ("Error during export: " <> pack (show (e :: SomeException))))


writeDrumKitFile' :: DrumkitPage -> Text -> FilePath -> IO ()
writeDrumKitFile' gui nm basepath = do
    dir <- createDrumgizmoDirectories basepath
    case dir of
        Left err -> displayErrorBox (guiDkParentWindow gui) ("Error during export: " <> err)
        Right () -> do
            desc <- getDkDescription gui

            -- read the drumkit from the IORef
            drumkit <- readIORef (guiDrumkit gui)

            case drumkit of
                Nothing -> return ()
                Just d -> do
                    channels <- listStoreToList (guiTvChannelsModel gui)
                    insts <- listStoreToList (guiTvInstrumentsModel gui)
                    basedir <- entryGetText (guiBaseDir gui)
                    let d' = d {dkName = nm, dkDescription = desc, dkChannels = channels, dkInstruments = insts}
                        --drumkitCont = convertToDrumkitXML d'
                        dgPath = getDrumgizmoDir basedir
                        drumkitFName = dgPath </> unpack nm <.> ".xml"

                    writeIORef (guiDrumkit gui) (Just d')

                    writeDrumKitXML d' drumkitFName
                    --B.writeFile drumkitFName drumkitCont

                    -- also export the instrument files
                    exportInstruments gui


exportInstruments :: DrumkitPage -> IO ()
exportInstruments gui = do
    v <- readIORef (guiDkInstrumentPages gui)
    vres <- V.forM v writeInstrumentFile

    if V.any isLeft vres
        then do
            let errs' = V.filter isLeft vres
                errs = lefts $ V.toList errs'
            displayMultiErrors (guiErrDiag gui) "Multiple Errors during export of Instrument Files:" errs
        else do
            displayInfoBox (guiDkParentWindow gui) "Successfully exported drumkit."



resetDrumkit :: DrumkitPage -> IO ()
resetDrumkit gui = do
    listStoreClear (guiTvChannelsModel gui)
    listStoreClear (guiTvInstrumentsModel gui)
    listStoreClear (guiTvChannelMapModel gui)

    writeIORef (guiDrumkit gui) Nothing

    v <- readIORef (guiDkInstrumentPages gui)
    V.mapM_ resetInstrumentPage v
    writeIORef (guiDkInstrumentPages gui) empty

    resetMidiMap (guiMidiMapGM gui)
    resetMidiMap (guiMidiMapDef gui)

    return ()
