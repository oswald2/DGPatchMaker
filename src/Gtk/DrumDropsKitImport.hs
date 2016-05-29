{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.DrumDropsKitImport
where


import Control.Monad as M (forM_)

import Graphics.UI.Gtk

import Data.Text as T
--import Data.IORef
--import Data.Vector as V
import Prelude as P

--import System.Directory
import System.FilePath.Find as F

import Data.List (length)

import Data.DrumDrops.Utils

import Data.IORef
import Data.Vector as V

import Gtk.MainWindow
import Gtk.InstrumentFrame



importDrumDropsDrumKit :: MainWindow InstrumentPage -> IO ()
importDrumDropsDrumKit gui = do
    b <- entryGetText (guiBaseDir gui) :: IO FilePath
    case b of
        "" -> displayErrorBox gui "Basedir must be set!"
        basedir -> do
            samplesDir <- entryGetText (guiSamplesDir gui)

            dirs <- getDirectoriesToImport samplesDir

            -- first clear the instruments notebook
            clearInstrumentsNotebook gui
            doImport basedir samplesDir dirs

            return ()
    where
        doImport :: FilePath -> FilePath -> [FilePath] -> IO ()
        doImport basedir samplesDir paths = do
            -- import the instruments
            let progress = guiProgress gui
                n = Data.List.length paths
                step :: Double
                step = 1.0 / fromIntegral n
            progressBarSetText progress ("Importing DrumDrops Drumkit..." :: Text)

            M.forM_ paths (doSingleImport progress basedir samplesDir step 0.0)

            progressBarSetText progress ("" :: Text)
            progressBarSetFraction progress 0.0

        doSingleImport progress basedir samplesDir step fraction path = do
            ins <- newInstrumentPage gui
            let instName = pathToInstrument samplesDir path
            _ <- notebookAppendPage (guiNotebookInstruments gui) (getMainBox ins) instName
            insertInstrumentPage gui ins

            res <- importInstrument basedir samplesDir path
            case res of
                Left err -> displayErrorBox gui err
                Right instFile -> do
                    setInstrumentFile ins instFile

                    -- update the progress bar
                    progressBarSetFraction progress (fraction + step)





clearInstrumentsNotebook :: MainWindow InstrumentPage -> IO ()
clearInstrumentsNotebook gui = do
    let nb = guiNotebookInstruments gui
    n <- notebookGetNPages nb
    M.forM_ [0..n] (notebookRemovePage nb)

    writeIORef (guiInstrumentPages gui) V.empty



getDirectoriesToImport :: FilePath -> IO [FilePath]
getDirectoriesToImport path = F.find recP filterP path
    where
        recP = always
        filterP = fileType ==? Directory
