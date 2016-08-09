{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Gtk.FileHandlingDialog
    (
    initFileHandlingDialog
    ,askUserForOverwriteIfNecessary
    ,FileHandlingDialog
    ,FhResultValue(..)
    ,resetFileHandlingDialog
    ,withFileHandlingDialog
    )
where


import Control.Monad (void)
import Control.Exception (bracket)
import Data.Text
import Graphics.UI.Gtk
import Data.IORef



data FileHandlingDialog = FileHandlingDialog {
    guiFhDialog :: MessageDialog,
    guiFhValue :: IORef FHandling
}

data FHandling =
    Skip
    | SkipAll
    | Overwrite
    | OverwriteAll
    deriving (Eq, Ord, Enum, Show)

data FhResultValue = SkipFile | OverwriteFile
    deriving (Eq, Ord, Enum, Show)


initFileHandlingDialog :: Builder -> IO FileHandlingDialog
initFileHandlingDialog builder = do
    messageDialog <- builderGetObject builder castToMessageDialog ("messagedialogFileHandling" :: Text)

    buttonSkip <- builderGetObject builder castToButton ("buttonSkip" :: Text)
    buttonSkipAll <- builderGetObject builder castToButton ("buttonSkipAll" :: Text)
    buttonOverwrite <- builderGetObject builder castToButton ("buttonOverwrite" :: Text)
    buttonOverwriteAll <- builderGetObject builder castToButton ("buttonOverwriteAll" :: Text)

    ir <- newIORef Overwrite

    let gui = FileHandlingDialog {
        guiFhDialog = messageDialog,
        guiFhValue = ir
        }

    void $ on buttonSkip buttonActivated $ setValue gui Skip
    void $ on buttonSkipAll buttonActivated $ setValue gui SkipAll
    void $ on buttonOverwrite buttonActivated $ setValue gui Overwrite
    void $ on buttonOverwriteAll buttonActivated $ setValue gui OverwriteAll

    return gui

setValue :: FileHandlingDialog -> FHandling -> IO ()
setValue gui handling = do
    writeIORef (guiFhValue gui) handling
    dialogResponse (guiFhDialog gui) (ResponseUser (fromEnum handling))

askUserForOverwriteIfNecessary :: FileHandlingDialog -> FilePath -> (IO ()) -> IO ()
askUserForOverwriteIfNecessary diag file writeAction = do
    let dialog = guiFhDialog diag
        txt = "File '" `append` pack file `append` "' does already exist."

    val <- readIORef (guiFhValue diag)

    let showDialog = do
            set dialog [messageDialogText := Just txt]
            (ResponseUser resp) <- dialogRun dialog
            widgetHide dialog
            case toEnum resp of
                Skip -> return SkipFile
                SkipAll -> return SkipFile
                Overwrite -> return OverwriteFile
                OverwriteAll -> return OverwriteFile

    v <- case val of
        Skip -> showDialog
        Overwrite -> showDialog
        SkipAll -> return SkipFile
        OverwriteAll -> return OverwriteFile

    case v of
        SkipFile -> return ()
        OverwriteFile -> writeAction



withFileHandlingDialog :: FileHandlingDialog -> (IO a) -> IO a
withFileHandlingDialog gui action = do
    bracket (resetFileHandlingDialog gui)
            (\_ -> resetFileHandlingDialog gui)
            (\_ -> action)


resetFileHandlingDialog :: FileHandlingDialog -> IO ()
resetFileHandlingDialog gui = writeIORef (guiFhValue gui) Overwrite

