{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.HitPowerDialog
    (
    HitPowerDialog
    ,initHitPowerDialog
    ,dialogGetHitParams
    ,dialogEqualEnable
    ,HitPowerDialogResult(..)
    )
where


import Control.Monad (void)
import Data.Text
import Graphics.UI.Gtk


data HitPowerDialog = HitPowerDialog {
    hpdStart :: SpinButton,
    hpdStop :: SpinButton,
    hpdStep :: SpinButton,
    hpdDialog :: Dialog
    }


data HitPowerDialogResult = HitPowerDialogResult {
    hprStart :: Int,
    hprStop :: Int,
    hprStep :: Int
    }

initHitPowerDialog :: Builder -> IO HitPowerDialog
initHitPowerDialog builder = do
    diag <- builderGetObject builder castToDialog ("dialogHitPowerLin" :: Text)
    start <- builderGetObject builder castToSpinButton ("spinbuttonStart" :: Text)
    stop <- builderGetObject builder castToSpinButton ("spinbuttonStop" :: Text)
    step <- builderGetObject builder castToSpinButton ("spinbuttonStep" :: Text)

    void $ dialogAddButton diag ("OK" :: Text) ResponseOk
    void $ dialogAddButton diag ("Cancel" :: Text) ResponseCancel


    let gui = HitPowerDialog {
        hpdDialog = diag,
        hpdStart = start,
        hpdStop = stop,
        hpdStep = step
        }

    return gui


dialogEqualEnable :: HitPowerDialog -> Bool -> IO ()
dialogEqualEnable gui val = do
    widgetSetSensitive (hpdStop gui) val
    widgetSetSensitive (hpdStep gui) val



dialogGetHitParams :: HitPowerDialog -> IO (Maybe HitPowerDialogResult)
dialogGetHitParams diag = do
    let
        dialog = hpdDialog diag

    resp <- dialogRun dialog
    widgetHide dialog
    case resp of
        ResponseOk -> do
            sta <- spinButtonGetValueAsInt (hpdStart diag)
            sto <- spinButtonGetValueAsInt (hpdStop diag)
            ste <- spinButtonGetValueAsInt (hpdStep diag)
            return $ Just (HitPowerDialogResult sta sto ste)
        _ -> return Nothing
