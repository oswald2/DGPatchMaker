{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.Drumkit

where


import Data.Types
import Data.Text as T
import Data.IORef
import Graphics.UI.Gtk

import Gtk.MainWindow


data DrumkitPage = DrumkitPage {
    guiTvChannels :: TreeView,
    guiTvInstruments :: TreeView,
    guiTvChannelsModel :: ListStore Microphones,
    guiTvInstrumentsModel :: ListStore ChannelMap,
    guiDrumkit :: IORef (Maybe Drumkit)
}


initDrumkitPage :: MainWindow a -> Builder -> IO DrumkitPage
initDrumkitPage mainWindow builder = do

    tvChannels <- builderGetObject builder castToTreeView ("treeviewChannels" :: Text)
    tvInstruments <- builderGetObject builder castToTreeView ("treeviewInstruments" :: Text)

    lsm <- listStoreNew []
    lsinst <- listStoreNew []
    dr <- newIORef Nothing

    let gui = DrumkitPage{
            guiTvChannels = tvChannels,
            guiTvInstruments = tvInstruments,
            guiTvChannelsModel = lsm,
            guiTvInstrumentsModel = lsinst,
            guiDrumkit = dr

        }

    return gui
