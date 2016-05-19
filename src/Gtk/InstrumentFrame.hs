{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.InstrumentFrame
    (
    InstrumentPage
    ,newInstrumentPage
    ,getMainBox
    )
where


import Prelude as P

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format

import Data.HitSample


data InstrumentPage = InstrumentPage {
    guiInstMainBox :: Box,
    guiInstHitView :: TreeView,
    guiInstHitViewModel :: ListStore HitSample,
    guiInstSamplesView :: TreeView,
    guiInstSamplesViewModel :: ListStore Sample
    }




newInstrumentPage :: IO InstrumentPage
newInstrumentPage = do
    -- Create the builder, and load the UI file
    builder <- builderNew

    builderAddFromFile builder "InstrumentPage.glade"
    --builderAddFromString builder builderFileAsString

    -- Retrieve some objects from the UI
    mainBox <- builderGetObject builder castToBox ("mainBox" :: Text)
    treeviewHit <- builderGetObject builder castToTreeView ("treeviewHit" :: Text)
    treeviewSamples <- builderGetObject builder castToTreeView ("treeviewSamples" :: Text)

    hsls <- listStoreNew []
    initTreeViewHit treeviewHit hsls

    sals <- listStoreNew []
    initTreeViewSamples treeviewSamples sals

    let gui = InstrumentPage {
        guiInstMainBox = mainBox,
        guiInstHitView = treeviewHit,
        guiInstHitViewModel = hsls,
        guiInstSamplesView = treeviewSamples,
        guiInstSamplesViewModel = sals
        }

    return gui


getMainBox :: InstrumentPage -> Box
getMainBox = guiInstMainBox


initTreeViewHit :: TreeView -> ListStore HitSample -> IO ()
initTreeViewHit tv ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Name" :: Text)
    treeViewColumnSetTitle col2 ("Hit Power" :: Text)

    renderer1 <- cellRendererTextNew
    renderer2 <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True

    cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [ cellText := hsName hs]
    cellLayoutSetAttributes col2 renderer2 ls $ \hs -> [ cellText := hsPowerAsString hs ]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `T.isPrefixOf` toLower (hsName row)

    return ()




initTreeViewSamples :: TreeView -> ListStore Sample -> IO ()
initTreeViewSamples tv ls = do
    treeViewSetModel tv ls

    treeViewSetHeadersVisible tv True

    -- add a couple columns
    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew
    col3 <- treeViewColumnNew

    treeViewColumnSetTitle col1 ("Channel" :: Text)
    treeViewColumnSetTitle col2 ("File" :: Text)
    treeViewColumnSetTitle col3 ("Filechannel" :: Text)

    renderer1 <- cellRendererTextNew
    renderer2 <- cellRendererTextNew
    renderer3 <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True
    cellLayoutPackStart col3 renderer3 True

    cellLayoutSetAttributes col1 renderer1 ls $ \hs -> [ cellText := saChannel hs]
    cellLayoutSetAttributes col2 renderer2 ls $ \hs -> [ cellText := saName hs ]
    cellLayoutSetAttributes col2 renderer2 ls $ \hs -> [ cellText := saFileChannelAsText hs ]

    _ <- treeViewAppendColumn tv col1
    _ <- treeViewAppendColumn tv col2

    treeViewSetEnableSearch tv True
    treeViewSetSearchEqualFunc tv $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath ls iter
        row <- listStoreGetValue ls i
        return $ toLower str `T.isPrefixOf` toLower (saName row)

    return ()
