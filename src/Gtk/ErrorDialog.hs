{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.ErrorDialog

where



import Data.Text
import Graphics.UI.Gtk


data ErrorDialog = ErrorDialog {
    guiEdDialog :: MessageDialog,
    guiEdTextView :: TextView
}


initErrorDialog :: Builder -> IO ErrorDialog
initErrorDialog builder = do
    messageDialog <- builderGetObject builder castToMessageDialog ("messagedialogCustom" :: Text)

    textview <- builderGetObject builder castToTextView ("textviewErrorMsgs" :: Text)

    let gui = ErrorDialog {
        guiEdDialog = messageDialog,
        guiEdTextView = textview
        }

    return gui


displayMultiErrors :: ErrorDialog -> Text -> [Text] -> IO ()
displayMultiErrors diag mainText errors = do
    let tv = guiEdTextView diag
        dialog = guiEdDialog diag
        errorText = intercalate "\n\nNext Error:\n\n" errors

    buffer <- textViewGetBuffer tv
    textBufferSetText buffer errorText

    set dialog [messageDialogText := Just mainText]

    _ <- dialogRun dialog
    widgetHide dialog

    return ()

