{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.NrHitsDialog
    (
    NrHitsDialog
    ,initNrHitsDialog
    ,dialogGetNrHits
    )
where


import Control.Monad (void)
import Data.Text
import Graphics.UI.Gtk


data NrHitsDialog = NrHitsDialog {
    nrhNumberHits :: SpinButton,
    nrhDialog :: Dialog
    }


initNrHitsDialog :: Builder -> IO NrHitsDialog
initNrHitsDialog builder = do
    nrHitsDiag <- builderGetObject builder castToDialog ("dialogNrHits" :: Text)
    spin <- builderGetObject builder castToSpinButton ("spinbuttonNrHits" :: Text)

    void $ dialogAddButton nrHitsDiag ("OK" :: Text) ResponseOk
    void $ dialogAddButton nrHitsDiag ("Cancel" :: Text) ResponseCancel


    let gui = NrHitsDialog {
        nrhDialog = nrHitsDiag,
        nrhNumberHits = spin
        }

    return gui



dialogGetNrHits :: NrHitsDialog -> IO (Maybe Int)
dialogGetNrHits diag = do
    let
        dialog = nrhDialog diag

    resp <- dialogRun dialog
    widgetHide dialog
    case resp of
        ResponseOk -> Just <$> spinButtonGetValueAsInt (nrhNumberHits diag)
        _ -> return Nothing
