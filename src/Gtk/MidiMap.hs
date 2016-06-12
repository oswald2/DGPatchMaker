{-# LANGUAGE OverloadedStrings, BangPatterns, NoImplicitPrelude #-}
module Gtk.MidiMap
    (
    MidiMapPage
    ,initMidiMap
    ,setMidiMap
    ,getMidiMapFromGUI
    )
where

import ClassyPrelude

import Data.Types
import Data.Checkers

import qualified Data.Text as T

import Graphics.UI.Gtk as G

import Gtk.Colors
import Gtk.Utils

import Debug.Trace


data MidiMapPage = MidiMapPage {
    mmMainWindow :: Window,
    mmMidiMapView :: TreeView,
    mmMidiMapModel :: ListStore MidiMapItem,
    mmNoteRenderer :: CellRendererText
}


data MidiMapItem = MidiMapItem {
    mmiNote :: Int,
    mmiInstrument :: Text,
    mmiIsOverlap :: Bool
} deriving Show

instance Eq MidiMapItem where
    x1 == x2 = (mmiNote x1) == (mmiNote x2)

instance Ord MidiMapItem where
    compare x1 x2 = compare (mmiNote x1) (mmiNote x2)


initMidiMap :: Window -> TreeView -> IO MidiMapPage
initMidiMap window tv = do

    ls <- listStoreNew []

    rend <- initTreeViewMM tv ls

    let gui = MidiMapPage {
            mmMainWindow = window,
            mmMidiMapModel = ls,
            mmMidiMapView = tv,
            mmNoteRenderer = rend
        }

    setupCallbacks gui

    return gui


initTreeViewMM :: TreeView -> ListStore MidiMapItem -> IO CellRendererText
initTreeViewMM tv ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Note" :: Text)
    treeViewColumnSetTitle col2 ("Instrument" :: Text)

    renderer1 <- cellRendererTextNew
    renderer2 <- cellRendererTextNew

    set renderer1 [cellTextEditable := True,
                    cellTextEditableSet := True
                    ]

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True


    cellLayoutSetAttributes col1 renderer1 ls $ \x -> [ cellText := T.pack (show (mmiNote x)),
                        cellTextBackgroundColor := yellow,
                        cellTextBackgroundSet := mmiIsOverlap x]
    cellLayoutSetAttributes col2 renderer2 ls $ \x -> [ cellText := mmiInstrument x]


    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        !row <- listStoreGetValue ls i
        return $ toLower str `isPrefixOf` toLower (mmiInstrument row)

    return renderer1


setMidiMap :: MidiMapPage -> MidiMap -> IO ()
setMidiMap gui mm = do
    let mmi = convertFromMM mm
        mmi' = checkOverlap mmi
        ls = mmMidiMapModel gui
    setListStoreTo ls mmi'

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
        overlaps = foldr f [] is'

        f :: (Int, MidiMapItem) -> [(Int, MidiMapItem)] -> [(Int, MidiMapItem)]
        f (idx1, mmi1) [] = [(idx1, mmi1)]
        f (idx1, mmi1) ((idx2, mmi2) : xs) =
            if mmiNote mmi1 == mmiNote mmi2
                then
                    let mmi1' = mmi1 {mmiIsOverlap = True}
                        mmi2' = mmi2 {mmiIsOverlap = True}
                    in (idx1, mmi1') : (idx2, mmi2') : xs
                else
                    let mmi1' = mmi1 {mmiIsOverlap = False}
                    in (idx1, mmi1') : (idx2, mmi2) : xs


        result = map snd $ sortOn fst overlaps
    in
    trace (printList result) result


printList :: Show a => [a] -> String
printList ls = intercalate "\n" $ map (pack.show) ls


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
                setListStoreTo model (checkOverlap ls)



