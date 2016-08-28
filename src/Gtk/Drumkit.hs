{-# LANGUAGE OverloadedStrings, BangPatterns, NoImplicitPrelude #-}
module Gtk.Drumkit

where


import ClassyPrelude

import Prelude (read)

import System.FilePath.Find as F
import System.FilePath

import Data.Types
import Data.DrumDrops.Utils
import Data.Char (isSpace, isDigit)
import Data.Export
import Data.Drumgizmo
import Data.Import
import Data.Either


import qualified Data.Vector as V

import Data.Text as T (last, dropEnd)
--import Data.IORef
import qualified Data.Set as S

import Graphics.UI.Gtk as G

import Gtk.Utils
import Gtk.Colors
import Gtk.InstrumentFrame
import Gtk.ErrorDialog
import Gtk.MidiMap
import Gtk.FileHandlingDialog


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
    guiTvChannelsModel :: ListStore Text,
    guiTvInstrumentsModel :: ListStore ChannelMap,
    guiTvChannelMapModel :: ListStore (Text, Text),
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
    guiBtCompileDef :: Button
}


initDrumkitPage :: Window ->
                    G.Builder ->
                    Notebook ->
                    ProgressBar ->
                    ComboBox ->
                    Entry ->
                    Entry ->
                    IORef (Vector InstrumentPage) ->
                    FileHandlingDialog ->
                    IO DrumkitPage
initDrumkitPage mainWindow builder instrumentsNotebook progress combo entryBaseDirectory entrySamplesDir ioref fhDialog = do

    buttonImportDrumkit <- builderGetObject builder castToButton ("buttonImportDrumkit" :: Text)
    -- buttonExportDrumkit <- builderGetObject builder castToButton ("buttonExportDrumkit" :: Text)
    buttonSetBaseDir <- builderGetObject builder castToButton ("buttonSetBaseDir" :: Text)
    buttonSetSamplesDir <- builderGetObject builder castToButton ("buttonSetSamplesDir" :: Text)
    buttonConvertToFullMix <- builderGetObject builder castToButton ("buttonConvertToFullMix" :: Text)

    miLoadDrumKit <- builderGetObject builder castToMenuItem ("imagemenuitemLoadDrumkit" :: Text)
    miExportDrumKit <- builderGetObject builder castToMenuItem ("menuitemExportDrumkit" :: Text)
    miSaveDrumkitFile <- builderGetObject builder castToMenuItem ("imagemenuitemSaveDrumkitFile" :: Text)


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

    channelRenderer <- initTvChannels tvChannels lsm
    groupRenderer <- initTvInstruments tvInstruments lsinst
    outRenderer <- initTvChannelMap tvChannelMap lsm lscm

    entrySetText entryBaseDirectory ("/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Multi Velocity Pack" :: FilePath)
    entrySetText entrySamplesDir ("/home/oswald/Sounds/Drumkits/2015_10_04_Mapex_Kit_AS_Pack_V2.3/Multi Velocity Pack/SAMPLES" :: FilePath)

    entrySetText eName ("MapexHeavyRockMultiVelocity" :: Text)

    gmLoadButton <- builderGetObject builder castToButton ("buttonLoadGMMap" :: Text)
    gmExportButton <- builderGetObject builder castToButton ("buttonExportGMMap" :: Text)

    defLoadButton <- builderGetObject builder castToButton ("buttonLoadDefMap" :: Text)
    defExportButton <- builderGetObject builder castToButton ("buttonExportDefMap" :: Text)

    resetButton <- builderGetObject builder castToButton ("buttonReset" :: Text)
    compileButton <- builderGetObject builder castToButton ("buttonCompile" :: Text)

    midiMapGm <- initMidiMap mainWindow tvMidiGM entryBaseDirectory gmLoadButton gmExportButton fhDialog
    midiMapDef <- initMidiMap mainWindow tvMidiDef entryBaseDirectory defLoadButton defExportButton fhDialog

    initParserCombo combo

    channelMenu <- builderGetObject builder castToMenu ("menuChannels" :: Text)
    itemAddChannel <- builderGetObject builder castToMenuItem ("menuitemAddChannel" :: Text)
    itemRemoveChannel <- builderGetObject builder castToMenuItem ("menuitemRemoveChannel" :: Text)

    channelMapMenu <- builderGetObject builder castToMenu ("menuChannelMap" :: Text)
    itemDuplicate <- builderGetObject builder castToMenuItem ("menuitemDuplicate" :: Text)
    itemRemoveCM <- builderGetObject builder castToMenuItem ("menuitemRemove" :: Text)

    btCompileGM <- builderGetObject builder castToButton ("buttonCompileFromKitGM" :: Text)
    btCompileDef <- builderGetObject builder castToButton ("buttonCompileFromKitDefault" :: Text)

    btUp <- builderGetObject builder castToButton ("buttonUp" :: Text)
    btDown <- builderGetObject builder castToButton ("buttonDown" :: Text)
    btSort <- builderGetObject builder castToButton ("buttonSort" :: Text)


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
            guiParserCombo = combo,
            guiChannelMenu = channelMenu,
            guiChannelRenderer = channelRenderer,
            guiFhDialog = fhDialog,
            guiOutChannelRenderer = outRenderer,
            guiGroupRenderer = groupRenderer,
            guiChannelMapMenu = channelMapMenu,
            guiBtCompileGM = btCompileGM,
            guiBtCompileDef = btCompileDef
        }

    setDkDescription gui "Mapex Heavy Rock Kit patch from the All Samples Pack from DrumDrops (http://www.drumdrops.com). Created by M. Oswald."

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
        str = map (pack . show) (enumFrom MapexParser)

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



setDrumkit :: DrumkitPage -> Drumkit -> IO ()
setDrumkit gui dk = do
    writeIORef (guiDrumkit gui) (Just dk)
    widgetSetSensitive (guiBtCompileGM gui) True
    widgetSetSensitive (guiBtCompileDef gui) True


clearDrumkit :: DrumkitPage -> IO ()
clearDrumkit gui = do
    writeIORef (guiDrumkit gui) Nothing
    widgetSetSensitive (guiBtCompileGM gui) False
    widgetSetSensitive (guiBtCompileDef gui) False


getDrumkit :: DrumkitPage -> IO (Maybe Drumkit)
getDrumkit gui = do
    readIORef (guiDrumkit gui)


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
                    setDrumkit gui drumkit

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
            ins <- instrumentPageNew (guiDkParentWindow gui) (guiDkInstrumentsNotebook gui)
                (guiBaseDir gui) (guiSamplesDir gui) (guiParserCombo gui) (guiDkInstrumentPages gui) (guiFhDialog gui) (guiErrDiag gui)
            let instName = pathToInstrument samplesDir path
            _ <- notebookAppendPage (guiDkInstrumentsNotebook gui) (instrumentPageGetMainBox ins) instName
            instrumentPageInsert ins

            pt <- comboBoxGetActiveText (guiParserCombo gui)
            let parserType = maybe MapexParser (read . unpack) pt

            res <- importInstrument parserType basedir samplesDir path
            case res of
                Left err -> do
                    displayErrorBox (guiDkParentWindow gui) err
                    return (Left err)
                Right instFile -> do
                    instrumentPageSetInstrumentFile ins instFile

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


initTvChannels :: TreeView -> ListStore Text -> IO (CellRendererText)
initTvChannels tv ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Channels" :: Text)

    renderer1 <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True

    set renderer1 [cellTextEditable := True,
                    cellTextEditableSet := True,
                    cellTextBackgroundColor := paleYellow,
                    cellTextBackgroundSet := True
                    ]

    cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [ cellText := hs]

    _ <- treeViewAppendColumn tv col1

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `isPrefixOf` toLower (show row)

    return (renderer1)


setChannels :: DrumkitPage -> [Text] -> IO ()
setChannels gui channels = setListStoreTo (guiTvChannelsModel gui) channels


initTvInstruments :: TreeView -> ListStore ChannelMap -> IO (CellRendererText)
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

    set renderer2 [cellTextEditable := True,
                    cellTextEditableSet := True]

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True
    cellLayoutPackStart col3 renderer3 True

    cellLayoutSetAttributes col1 renderer1 ls $ \cm -> [ cellText := cmName cm,
            cellTextBackgroundColor := yellow,
            cellTextBackgroundSet := cmContainsUndefined cm]
    cellLayoutSetAttributes col2 renderer2 ls $ \cm -> [ cellText := maybe "--" id (cmGroup cm),
            cellTextBackgroundColor := yellow,
            cellTextBackgroundSet := cmContainsUndefined cm]
    cellLayoutSetAttributes col3 renderer3 ls $ \cm -> [ cellText := cmFile cm,
            cellTextBackgroundColor := yellow,
            cellTextBackgroundSet := cmContainsUndefined cm]


    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2
    _ <- treeViewAppendColumn tv col3

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `isPrefixOf` toLower (cmName row)

    return (renderer2)

setInstruments :: DrumkitPage -> [ChannelMap] -> IO ()
setInstruments gui insts = setListStoreTo (guiTvInstrumentsModel gui) insts


initTvChannelMap :: TreeView -> ListStore Text -> ListStore (Text, Text) -> IO (CellRendererCombo)
initTvChannelMap tv lsMicros ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("In" :: Text)
    treeViewColumnSetTitle col2 ("Out" :: Text)

    renderer1 <- cellRendererTextNew
    renderer2 <- cellRendererComboNew

    let colId :: ColumnId Text Text
        colId = makeColumnIdString 0
        -- in this case we take the outputs liststore

    outChans <- listStoreToList lsMicros
    lsChans <- listStoreNew outChans

    treeModelSetColumn lsChans colId id

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True

    set renderer2 [ cellTextEditable := True,
                    cellTextEditableSet := True,
                    cellTextBackgroundColor := paleYellow,
                    cellTextBackgroundSet := True,

                    cellComboTextModel := (lsChans, colId)
                    ]

    cellLayoutSetAttributes col1 renderer1 ls $ \(i, _) -> [ cellText := i]
    cellLayoutSetAttributes col2 renderer2 ls $ \(_, o) -> [ cellText := o, cellComboTextModel := (lsChans, colId)]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        (x, _) <- listStoreGetValue ls i
        return $ toLower str `isPrefixOf` toLower x

    return (renderer2)



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
    void $ G.on (guiOutChannelRenderer gui) editingStarted $ \widget treepath -> do
        case treepath of
            [_] -> do
                comboListStore <- comboBoxSetModelText (castToComboBox widget)
                outChans <- listStoreToList (guiTvChannelsModel gui)
                void $ mapM (listStoreAppend comboListStore) (outChans :: [Text])
                return ()
            _ -> return ()

    -- callback for editing the channel
    void $ G.on (guiOutChannelRenderer gui) edited $ \[i] str -> do
        (inc, _) <- listStoreGetValue (guiTvChannelMapModel gui) i
        -- set the GTK list store to the new value
        let val' = (inc, str)
        listStoreSetValue (guiTvChannelMapModel gui) i val'
        -- we also need to set the new value in the instrument itself

        sel <- treeViewGetSelection (guiTvInstruments gui)
        path <- treeSelectionGetSelectedRows sel
        case path of
            ((idx:_) : _) -> do
                hsVal <- listStoreGetValue (guiTvInstrumentsModel gui) idx
                let cm = cmMap hsVal
                    cm' = zipWith upd cm [0..]
                    upd s j = if j == i then val' else s
                    hsVal' = hsVal {cmMap = cm'}
                listStoreSetValue (guiTvInstrumentsModel gui) idx hsVal'
            _ -> return ()

    -- edit call back for editing the channels
    void $ G.on (guiGroupRenderer gui) edited $ \[i] str -> do
        oldVal <- listStoreGetValue (guiTvInstrumentsModel gui) i
        -- set the GTK list store to the new value
        let newVal = if str == "" || str == "--" then oldVal {cmGroup = Nothing} else oldVal {cmGroup = Just str}
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
    case null basepath of
        True -> displayErrorBox (guiDkParentWindow gui) "No basepath specified!"
        False -> do
            nm <- getDkName gui
            case null nm of
                True -> displayErrorBox (guiDkParentWindow gui) "No drumkit name specified!"
                False -> withFileHandlingDialog (guiFhDialog gui) $ do
                            writeDrumKitFile gui nm basepath
                            -- get the midi map and write it
                            --gmMidi <- getMidiMapFromGUI (guiMidiMapGM gui)
                            --defMidi <- getMidiMapFromGUI (guiMidiMapDef gui)

                            --writeMidiMapFile (guiMidiMapGM gui) "MIDIMap_GM.xml" gmMidi
                            --writeMidiMapFile (guiMidiMapDef gui) "MIDIMap_Default.xml" defMidi



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
            drumkit <- getDrumkit gui

            case drumkit of
                Nothing -> return ()
                Just d -> do
                    channels <- listStoreToList (guiTvChannelsModel gui)
                    insts <- listStoreToList (guiTvInstrumentsModel gui)
                    basedir <- entryGetText (guiBaseDir gui)
                    let d' = d {dkName = nm, dkDescription = desc, dkChannels = channels, dkInstruments = insts}
                        dgPath = getDrumgizmoDir basedir
                        drumkitFName = dgPath </> unpack nm <.> ".xml"

                    setDrumkit gui d'

                    askUserForOverwriteIfNecessary (guiFhDialog gui) drumkitFName $ writeDrumKitXML d' drumkitFName

                    -- also export the instrument files
                    exportInstruments gui



saveDrumkit :: DrumkitPage -> IO ()
saveDrumkit gui = do
    basepath <- entryGetText (guiBaseDir gui)
    nm <- unpack <$> getDkName gui

    let parentWindow = guiDkParentWindow gui

    dialog <- fileChooserDialogNew
                (Just $ ("Save Drumkit File" :: Text))             --dialog title
                (Just parentWindow)                     --the parent window
                FileChooserActionSave                         --the kind of dialog we want
                [("gtk-cancel"                                --The buttons to display
                 ,ResponseCancel)
                 ,("gtk-save"
                 , ResponseAccept)]

    void $ fileChooserSetCurrentFolder dialog (getDrumgizmoDir basepath)
    void $ fileChooserSetCurrentName dialog nm


    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept -> do
            nam <- fileChooserGetFilename dialog
            bp <- fileChooserGetCurrentFolder dialog

            case (nam, bp) of
                (Just name, Just dir) -> do
                    withFileHandlingDialog (guiFhDialog gui) $ do
                        drumkit <- getDrumkit gui
                        case drumkit of
                            Nothing -> return ()
                            Just d -> do
                                channels <- listStoreToList (guiTvChannelsModel gui)
                                insts <- listStoreToList (guiTvInstrumentsModel gui)
                                desc <- getDkDescription gui
                                let d' = d {dkName = (pack nm), dkDescription = desc, dkChannels = channels, dkInstruments = insts}
                                    drumkitFName' = dir </> name
                                    drumkitFName = if takeExtension drumkitFName' == ".xml" then drumkitFName' else addExtension drumkitFName' ".xml"
                                setDrumkit gui d'

                                askUserForOverwriteIfNecessary (guiFhDialog gui) drumkitFName $ writeDrumKitXML d' drumkitFName
                _ -> return ()
        _ -> return ()
    widgetHide dialog



exportInstruments :: DrumkitPage -> IO ()
exportInstruments gui = do
    v <- readIORef (guiDkInstrumentPages gui)
    vres <- V.forM v instrumentPageWriteInstrumentFile

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

    clearDrumkit gui

    v <- readIORef (guiDkInstrumentPages gui)
    V.mapM_ instrumentPageReset v
    writeIORef (guiDkInstrumentPages gui) empty

    resetMidiMap (guiMidiMapGM gui)
    resetMidiMap (guiMidiMapDef gui)

    return ()


compileDrumkit :: DrumkitPage -> IO ()
compileDrumkit gui = do
    inst <- readIORef (guiDkInstrumentPages gui)

    instF <- V.mapM instrumentPageGetInstrumentFile inst

    let errs = lefts (V.toList instF)
    if not (null errs)
        then do
            displayMultiErrors (guiErrDiag gui) "Multiple errors happened during Import of Instruments:" errs
            return ()
        else do
            nm <- getDkName gui
            desc <- getDkDescription gui

            let drumkit = generateDrumkit nm desc insts
                insts = rights (V.toList instF)

            -- set the actual drumkit
            setDrumkit gui drumkit

            -- set the channels for viewing
            setChannels gui (dkChannels drumkit)
            setInstruments gui (dkInstruments drumkit)

            -- also convert the drumkit to a midi map
            let midimap = getMidiMap drumkit

            setMidiMap (guiMidiMapGM gui) midimap
            setMidiMap (guiMidiMapDef gui) midimap


addChannel :: DrumkitPage -> IO ()
addChannel gui = do
    let def = pack (showMic Undefined)
    idx <- listStoreAppend (guiTvChannelsModel gui) def
    treeViewSetCursor (guiTvChannels gui) [idx] Nothing
    activateRow (guiTvChannels gui) idx


removeChannel :: DrumkitPage -> IO ()
removeChannel gui = do
    sel <- treeViewGetSelection (guiTvChannels gui)
    path <- treeSelectionGetSelectedRows sel
    case path of
        ((idx:_) : _) -> do
            chan <- listStoreGetValue (guiTvChannelsModel gui) idx
            listStoreRemove (guiTvChannelsModel gui) idx
            let def = pack (showMic Undefined)
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
    when (not (null undef)) $ do
        ls <- listStoreToList (guiTvChannelsModel gui)
        let def = pack (showMic Undefined)
        case ClassyPrelude.find (== def) ls of
            Nothing -> addChannel gui  -- if we don't find Undefined and we need it
                                       -- because there are undefined channel mappings
                                       -- we have to re-add it
            Just _ -> return ()



loadDrumkit :: DrumkitPage -> IO ()
loadDrumkit gui = do
    let parentWindow = guiDkParentWindow gui
    dialog <- fileChooserDialogNew
              (Just $ ("Load a Drumkit" :: Text))             --dialog title
              (Just parentWindow)                     --the parent window
              FileChooserActionOpen                         --the kind of dialog we want
              [("gtk-cancel"                                --The buttons to display
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]

    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept -> do
            f <- fileChooserGetFilename dialog
            case f of
                Nothing -> return ()
                Just file -> do
                    loadDrumkit' gui file
                    return ()
        _ -> return ()
    widgetHide dialog


loadDrumkit' :: DrumkitPage -> FilePath -> IO ()
loadDrumkit' gui file = do
    res <- importDrumkitFile file
    case res of
        Left err -> displayErrorBox (guiDkParentWindow gui) ("Error on loading drumkit: " <> err)
        Right dk -> do
            resetDrumkit gui

            let basepath = getBasePath file
                basepathT :: Text
                basepathT = pack basepath
            entrySetText (guiBaseDir gui) basepathT
            entrySetText (guiSamplesDir gui) basepathT

            entrySetText (guiDkName gui) (dkName dk)
            setDkDescription gui (dkDescription dk)

            -- set the actual drumkit
            setDrumkit gui dk

            -- set the channels for viewing
            setChannels gui (dkChannels dk)
            setInstruments gui (dkInstruments dk)

            -- now load the instrument files
            loadInstrumentFiles gui (takeDirectory file) (dkInstruments dk)
            return ()


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
            void $ notebookAppendPage (guiDkInstrumentsNotebook gui) (instrumentPageGetMainBox ins) name
            instrumentPageInsert ins
            instrumentPageSetInstrumentName ins name
            instrumentPageLoadFile ins (path </> (cmFile cm))



convertToFullMix :: DrumkitPage -> IO ()
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
                                | otherwise = (inc, pack (showMic FullMixL)) : (inc, pack (showMic FullMixR)) : func xs

isLeftChannel :: Text -> Bool
isLeftChannel x | T.last x == 'L' = True
         | isDigit (T.last x) && (T.last (T.dropEnd 1 x)) == 'L' = True
         | otherwise = False

isRightChannel :: Text -> Bool
isRightChannel x | T.last x == 'R' = True
         | isDigit (T.last x) && (T.last (T.dropEnd 1 x)) == 'R' = True
         | otherwise = False



duplicateCM :: DrumkitPage -> IO ()
duplicateCM gui = do
    -- get seledted cm and duplicate it
    sel <- treeViewGetSelection (guiTvChannelMap gui)
    s <- treeSelectionGetSelectedRows sel
    case s of
        ((x:_):_) -> do
            val <- listStoreGetValue (guiTvChannelMapModel gui) x
            listStoreInsert (guiTvChannelMapModel gui) (x + 1) val

            sel1 <- treeViewGetSelection (guiTvInstruments gui)
            s1 <- treeSelectionGetSelectedRows sel1
            case s1 of
                ((i:_):_) -> do
                    cm <- listStoreGetValue (guiTvInstrumentsModel gui) i
                    vals <- listStoreToList (guiTvChannelMapModel gui)
                    listStoreSetValue (guiTvInstrumentsModel gui) i (cm {cmMap = vals})
                _ -> return ()

        _ -> return ()


removeCM :: DrumkitPage -> IO ()
removeCM gui = do
    sel <- treeViewGetSelection (guiTvChannelMap gui)
    s <- treeSelectionGetSelectedRows sel
    case s of
        ((x:_):_) -> do
            listStoreRemove (guiTvChannelMapModel gui) x

            sel1 <- treeViewGetSelection (guiTvInstruments gui)
            s1 <- treeSelectionGetSelectedRows sel1
            case s1 of
                ((i:_):_) -> do
                    cm <- listStoreGetValue (guiTvInstrumentsModel gui) i
                    vals <- listStoreToList (guiTvChannelMapModel gui)
                    listStoreSetValue (guiTvInstrumentsModel gui) i (cm {cmMap = vals})
                _ -> return ()
        _ -> return ()


compileMidiMapGM :: DrumkitPage -> IO ()
compileMidiMapGM gui = do
    -- also convert the drumkit to a midi map
    dr <- getDrumkit gui
    case dr of
        Nothing -> return ()
        Just drumkit -> do
            let midimap = getMidiMap drumkit
            setMidiMap (guiMidiMapGM gui) midimap


compileMidiMapDefault :: DrumkitPage -> IO ()
compileMidiMapDefault gui = do
    -- also convert the drumkit to a midi map
    dr <- getDrumkit gui
    case dr of
        Nothing -> return ()
        Just drumkit -> do
            let midimap = getMidiMap drumkit
            setMidiMap (guiMidiMapDef gui) midimap


channelUp :: DrumkitPage -> IO ()
channelUp gui = do
    sel1 <- treeViewGetSelection (guiTvChannels gui)
    s1 <- treeSelectionGetSelectedRows sel1
    case s1 of
        ((i:_):_) -> do
            let ls = guiTvChannelsModel gui
            ch <- listStoreGetValue ls i
            listStoreRemove ls i
            let idx = if i > 0 then (i - 1) else 0
            listStoreInsert ls idx ch
            activateRow (guiTvChannels gui) idx
        _ -> return ()


channelDown :: DrumkitPage -> IO ()
channelDown gui = do
    sel1 <- treeViewGetSelection (guiTvChannels gui)
    s1 <- treeSelectionGetSelectedRows sel1
    case s1 of
        ((i:_):_) -> do
            let ls = guiTvChannelsModel gui
            ch <- listStoreGetValue ls i
            listStoreRemove ls i
            let idx = (i + 1)
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
    let !xx = toLower x
        worker
            | "kick" `isInfixOf` xx = (0, x)
            | "snare" `isInfixOf` xx = (1, x)
            | "hihat" `isInfixOf` xx = (2, x)
            | "tom" `isInfixOf` xx = (3, x)
            | "floor" `isInfixOf` xx = (4, x)
            | "ride" `isInfixOf` xx = (5, x)
            | "ohl" == xx = (6, x)
            | "ohr" == xx = (6, x)
            | "room" `isInfixOf` xx = (7, x)
            | "fullmix" `isInfixOf` xx = (8, x)
            | otherwise = (100, x)
    in
        worker
