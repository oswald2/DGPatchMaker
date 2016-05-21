module Data.Drumgizmo

where


import Data.Text as T

import Control.Exception
import Control.Monad (when)

import System.FilePath
import System.Directory



getDrumgizmoDir :: FilePath -> FilePath
getDrumgizmoDir path = path </> "DRUMGIZMO"

getInstrumentDir :: FilePath -> FilePath
getInstrumentDir path = getDrumgizmoDir path </> "Instruments"

createDrumgizmoDirectories :: FilePath -> IO (Either Text ())
createDrumgizmoDirectories path = do
    catch (do
        let dgPath = getDrumgizmoDir path
        e <- doesDirectoryExist dgPath
        when (not e) $ createDirectory dgPath

        let instPath = getInstrumentDir path
        e1 <- doesDirectoryExist instPath
        when (not e1) $ createDirectory instPath

        return (Right ())
        )
        (\e -> do
                let err = show (e :: SomeException)
                return (Left (pack err))
        )
