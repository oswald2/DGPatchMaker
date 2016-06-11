{-# LANGUAGE OverloadedStrings, BangPatterns, NoImplicitPrelude #-}
module Gtk.Drumkit

where


import ClassyPrelude

--import Control.Monad (void)
--import Control.Exception

import System.FilePath.Find as F
import System.FilePath

import Data.Types
import Data.DrumDrops.Utils

--import Data.Text as T
--import Data.IORef
import qualified Data.Set as S

import Graphics.UI.Gtk as G

import Gtk.Utils
import Gtk.InstrumentFrame



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
    guiDrumkit :: IORef (Maybe Drumkit)
}


initDrumkitPage :: Window -> G.Builder -> Notebook -> ProgressBar -> Entry -> Entry -> IO DrumkitPage
initDrumkitPage mainWindow builder instrumentsNotebook progress entryBaseDirectory entrySamplesDir = do

    buttonImportDrumkit <- builderGetObject builder castToButton ("buttonImportDrumkit" :: Text)
    buttonSetBaseDir <- builderGetObject builder castToButton ("buttonSetBaseDir" :: Text)
    buttonSetSamplesDir <- builderGetObject builder castToButton ("buttonSetSamplesDir" :: Text)

    tvChannels <- builderGetObject builder castToTreeView ("treeviewChannels" :: Text)
    tvInstruments <- builderGetObject builder castToTreeView ("treeviewInstruments" :: Text)
    tvChannelMap <- builderGetObject builder castToTreeView ("treeviewChannelMap" :: Text)

    eName <- builderGetObject builder castToEntry ("entryDkName" :: Text)
    eDesc <- builderGetObject builder castToTextView ("textviewDkDescripton" :: Text)

    lsm <- listStoreNew []
    lsinst <- listStoreNew []
    lscm <- listStoreNew []

    dr <- newIORef Nothing

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
            guiDkProgress = progress
        }

    void $ G.on buttonSetBaseDir buttonActivated $ setBaseDir gui
    void $ G.on buttonSetSamplesDir buttonActivated $ setSamplesDir gui

    --void $ on buttonImportDrumkit buttonActivated $ importDrumDropsDrumKit gui

    return gui


getDkName :: DrumkitPage -> IO Text
getDkName dkp = entryGetText (guiDkName dkp)


getDkDescription :: DrumkitPage -> IO Text
getDkDescription dkp = do
    buffer <- textViewGetBuffer (guiDkDescription dkp)
    (start, end) <- textBufferGetBounds buffer
    textBufferGetText buffer start end False





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
    catch (importDrumDropsDrumKit' gui)
        (\e -> displayErrorBox (guiDkParentWindow gui) ("Error: " <> pack (show (e :: SomeException))))



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

            case null (lefts instFiles) of
                False -> do
                    -- TODO: implement error handling
                    return ()
                True -> do
                    let insts = rights instFiles
                    nm <- getDkName gui
                    desc <- getDkDescription gui

                    let drumkit = generateDrumkit nm desc insts
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

            instruments <- forM paths (doSingleImport progress basedir samplesDir step 0.0)

            progressBarSetText progress ("" :: Text)
            progressBarSetFraction progress 0.0

            return instruments

        doSingleImport progress basedir samplesDir step fraction path = do
            ins <- newInstrumentPage (guiDkParentWindow gui) (guiDkInstrumentsNotebook gui)
                (guiBaseDir gui) (guiSamplesDir gui)
            let instName = pathToInstrument samplesDir path
            _ <- notebookAppendPage (guiDkInstrumentsNotebook gui) (getMainBox ins) instName
            --insertInstrumentPage gui ins

            res <- importInstrument basedir samplesDir path
            case res of
                Left err -> do
                    displayErrorBox (guiDkParentWindow gui) err
                    return (Left err)
                Right instFile -> do
                    setInstrumentFile ins instFile

                    -- update the progress bar
                    progressBarSetFraction progress (fraction + step)
                    return (Right instFile)





getDirectoriesToImport :: FilePath -> IO [FilePath]
getDirectoriesToImport path = do
    dirs <- F.find recP filterP path
    let s :: Set FilePath
        s = S.fromList $ map takeDirectory dirs
    return (S.toList s)
    where
        recP = always
        filterP = extension ==? ".wav"




