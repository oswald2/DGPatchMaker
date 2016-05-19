{-# LANGUAGE OverloadedStrings #-}
module Gtk.InstrumentFrame
    (
    InstrumentPage
    ,newInstrumentPage
    ,getMainBox
    )
where



import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Data.Text


data InstrumentPage = InstrumentPage {
    guiInstMainBox :: Box
    }



newInstrumentPage :: IO InstrumentPage
newInstrumentPage = do
    -- Create the builder, and load the UI file
    builder <- builderNew

    builderAddFromFile builder "InstrumentPage.glade"
    --builderAddFromString builder builderFileAsString

    -- Retrieve some objects from the UI
    mainBox <- builderGetObject builder castToBox ("mainBox" :: Text)

    let gui = InstrumentPage {
        guiInstMainBox = mainBox}

    return gui


getMainBox :: InstrumentPage -> Box
getMainBox = guiInstMainBox
