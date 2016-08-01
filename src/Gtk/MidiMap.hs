{-# LANGUAGE OverloadedStrings, BangPatterns, NoImplicitPrelude #-}
module Gtk.MidiMap
    (
    MidiMapPage
    ,initMidiMap
    ,setMidiMap
    ,getMidiMapFromGUI
    ,writeMidiMapFile
    ,resetMidiMap
    )
where

import ClassyPrelude

import Data.Types
import Data.Checkers
import Data.Drumgizmo
import Data.Import
import Data.Export

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L
--import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V

import Graphics.UI.Gtk as G

import Gtk.Colors
import Gtk.Utils

import System.FilePath

--import Debug.Trace


data MidiMapPage = MidiMapPage {
    mmMainWindow :: Window,
    mmMidiMapView :: TreeView,
    mmMidiMapModel :: ListStore MidiMapItem,
    mmNoteRenderer :: CellRendererText,
    mmLoadMap :: Button,
    mmExportMap :: Button,
    mmBasePath :: Entry
}


data MidiMapItem = MidiMapItem {
    mmiNote :: Int,
    mmiInstrument :: Text,
    mmiIsOverlap :: Bool
} deriving (Show, Eq)

--instance Eq MidiMapItem where
--    x1 == x2 = (mmiNote x1) == (mmiNote x2)

instance Ord MidiMapItem where
    compare x1 x2 = compare (mmiNote x1) (mmiNote x2)


initMidiMap :: Window -> TreeView -> Entry -> Button -> Button -> IO MidiMapPage
initMidiMap window tv basepath loadButton exportButton = do

    ls <- listStoreNew []

    rend <- initTreeViewMM tv ls

    let gui = MidiMapPage {
            mmMainWindow = window,
            mmMidiMapModel = ls,
            mmMidiMapView = tv,
            mmNoteRenderer = rend,
            mmLoadMap = loadButton,
            mmExportMap = exportButton,
            mmBasePath = basepath
        }

    setupCallbacks gui

    void $ G.on loadButton buttonActivated $ cbImportMidiMap gui
    void $ G.on exportButton buttonActivated $ cbExportMidiMap gui

    return gui


initTreeViewMM :: TreeView -> ListStore MidiMapItem -> IO CellRendererText
initTreeViewMM tv ls = do
    sortModel <- treeModelSortNewWithModel ls

    treeViewSetModel tv sortModel

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew
    col3 <- treeViewColumnNew

    let sort0 = 0
        sort1 = 1
        sort2 = 2

    treeViewColumnSetSortColumnId col1 sort0
    treeViewColumnSetSortColumnId col2 sort1
    treeViewColumnSetSortColumnId col3 sort2

    treeViewColumnSetTitle col1 ("MIDI" :: Text)
    treeViewColumnSetTitle col2 ("Note" :: Text)
    treeViewColumnSetTitle col3 ("Instrument" :: Text)

    renderer1 <- cellRendererTextNew
    renderer2 <- cellRendererTextNew
    renderer3 <- cellRendererTextNew

    set renderer1 [cellTextEditable := True,
                    cellTextEditableSet := True
                    ]

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True
    cellLayoutPackStart col3 renderer3 True


    cellLayoutSetAttributes col1 renderer1 ls $ \x -> [ cellText := T.pack (show (mmiNote x)),
                        cellTextBackgroundColor := yellow,
                        cellTextBackgroundSet := mmiIsOverlap x]
    cellLayoutSetAttributes col2 renderer2 ls $ \x -> [ cellText := midiToNote (mmiNote x),
                        cellTextBackgroundColor := yellow,
                        cellTextBackgroundSet := mmiIsOverlap x]
    cellLayoutSetAttributes col3 renderer3 ls $ \x -> [ cellText := mmiInstrument x]


    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2
    _ <- treeViewAppendColumn tv col3

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        res <- treeModelGetPath ls iter
        if (not (null res))
            then do
                let (i : _) = res
                !row <- listStoreGetValue ls i
                return $ toLower str `isInfixOf` toLower (mmiInstrument row)
            else return False

    treeSortableSetSortFunc sortModel sort0 $ \iter1 iter2 -> do
        m1 <- treeModelGetRow ls iter1
        m2 <- treeModelGetRow ls iter2
        return (compare (mmiNote m1) (mmiNote m2))
    treeSortableSetSortFunc sortModel sort1 $ \iter1 iter2 -> do
        m1 <- treeModelGetRow ls iter1
        m2 <- treeModelGetRow ls iter2
        return (compare (mmiNote m1) (mmiNote m2))
    treeSortableSetSortFunc sortModel sort2 $ \iter1 iter2 -> do
        m1 <- treeModelGetRow ls iter1
        m2 <- treeModelGetRow ls iter2
        return (compare (mmiInstrument m1) (mmiInstrument m2))


    return renderer1


setMidiMap :: MidiMapPage -> MidiMap -> IO ()
setMidiMap gui mm = do
    let ls = mmMidiMapModel gui
    setListStoreTo ls ((checkOverlap.convertFromMM) mm)

getMidiMapFromGUI :: MidiMapPage -> IO MidiMap
getMidiMapFromGUI gui = do
    ls <- listStoreToList (mmMidiMapModel gui)
    return $! (convertToMM ls)


convertFromMM :: MidiMap -> [MidiMapItem]
convertFromMM mm = map conv (mmNote mm)
    where
        conv (note, inst) = MidiMapItem note inst False

convertToMM :: [MidiMapItem] -> MidiMap
convertToMM ls = MidiMap (map conv ls)
    where
        conv mm = (mmiNote mm, mmiInstrument mm)


checkOverlap :: [MidiMapItem] -> [MidiMapItem]
checkOverlap mm =
    let is :: [(Int, MidiMapItem)]
        is = zip [0..] mm
        is' = sortOn snd is

        pred' (_, mm1) (_, mm2) = mmiNote mm1 == mmiNote mm2
        gr = groupBy pred' is'
        overlaps = concat $ map f gr
        f [] = []
        f [(idx, mmi)] = [(idx, mmi {mmiIsOverlap = False})]
        f l@(_ : _) = map overlap l
        overlap (idx, mmi) = (idx, mmi {mmiIsOverlap = True})

        result = map snd $ sortOn fst overlaps
    in
    result


--printList :: Show a => [a] -> String
--printList ls = intercalate "\n" $ map (pack.show) ls


setupCallbacks :: MidiMapPage -> IO ()
setupCallbacks gui = do
    let model = mmMidiMapModel gui

    void $ G.on (mmNoteRenderer gui) edited $ \[i] str -> do
        val <- listStoreGetValue model i
        let res = checkInt str 0 127 (Just "Illegal Value for MIDI Note")
        case res of
            Left err -> displayErrorBox (mmMainWindow gui) err
            Right x -> do
                let val' = val {mmiNote = fromIntegral x}
                listStoreSetValue model i val'

                -- check for overlaps
                ls <- listStoreToList model
                let vec = V.fromList $ checkOverlap ls
                n <- listStoreGetSize model
                forM_ [0..(n-1)] $ \j -> do
                    m <- listStoreGetValue model j
                    let vm = vec V.! j
                    if (m /= vm) then listStoreSetValue model j vm else return ()



cbImportMidiMap :: MidiMapPage -> IO ()
cbImportMidiMap gui = do
    let parentWindow = mmMainWindow gui
    dialog <- fileChooserDialogNew
              (Just $ ("Select MIDI Map for Loading" :: Text))             --dialog title
              (Just parentWindow)                     --the parent window
              FileChooserActionOpen                         --the kind of dialog we want
              [("gtk-cancel"                                --The buttons to display
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]
    basepath <- entryGetText (mmBasePath gui)
    let path = getDrumgizmoDir basepath
    void $ fileChooserSetFilename dialog path

    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept -> do
            Just file <- fileChooserGetFilename dialog
            res <- importMidiMap file
            case res of
                Nothing -> displayErrorBox parentWindow ("Could not load MIDI Map: " `T.append` (pack file))
                Just x -> setMidiMap gui x
            return ()
        ResponseCancel -> return ()
        ResponseDeleteEvent -> return ()
        _ -> return ()
    widgetHide dialog


cbExportMidiMap :: MidiMapPage -> IO ()
cbExportMidiMap gui = do
    let parentWindow = mmMainWindow gui
    dialog <- fileChooserDialogNew
              (Just $ ("Save MIDI Map" :: Text))             --dialog title
              (Just parentWindow)                     --the parent window
              FileChooserActionSave                         --the kind of dialog we want
              [("gtk-cancel"                                --The buttons to display
               ,ResponseCancel)
              ,("gtk-save"
               , ResponseAccept)]
    basepath <- entryGetText (mmBasePath gui)
    let path = getDrumgizmoDir basepath
    void $ fileChooserSetFilename dialog path

    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept -> do
            Just file <- fileChooserGetFilename dialog
            -- export the file
            midimap <- getMidiMapFromGUI gui
            writeMidiMapFile gui (pack file) midimap
            return ()
        ResponseCancel -> return ()
        ResponseDeleteEvent -> return ()
        _ -> return ()
    widgetHide dialog





writeMidiMapFile :: MidiMapPage -> Text -> MidiMap -> IO ()
writeMidiMapFile gui filename midimap = do
    catch (writeMidiMapFile' gui filename midimap)
        (\e -> displayErrorBox (mmMainWindow gui) ("Error during MIDI map export: " <> pack (show (e :: SomeException))))

writeMidiMapFile' :: MidiMapPage -> Text -> MidiMap -> IO ()
writeMidiMapFile' gui filename midimap = do
    basepath <- entryGetText (mmBasePath gui)
    let --content = convertToMidiMapXML midimap
        content2 = convertToTabSep midimap
        path = getDrumgizmoDir basepath </> unpack filename
        path2 = replaceExtension path ".txt"

    --B.writeFile path content
    writeMidiMapXML midimap path
    L.writeFile path2 content2




resetMidiMap :: MidiMapPage -> IO ()
resetMidiMap gui = do
    listStoreClear (mmMidiMapModel gui)
    return ()

