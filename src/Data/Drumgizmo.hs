{-# LANGUAGE OverloadedStrings #-}
module Data.Drumgizmo

where


import Data.Text as T
import Data.List as L (dropWhileEnd, foldr1)
import Control.Exception
import Control.Monad (when)

import System.FilePath
import System.Directory


drName :: FilePath
drName = "DRUMGIZMO"

-- | given the base path returns the path to the DRUMGIZMO directory
getDrumgizmoDir :: FilePath -> FilePath
getDrumgizmoDir path = path </> drName

-- | given the base path returns the Instrumetns directory
getInstrumentDir :: FilePath -> FilePath
getInstrumentDir path = getDrumgizmoDir path </> "Instruments"


-- | gets the base path from a drumkit file name
getBasePath :: FilePath -> FilePath
getBasePath drumkitFile = (L.foldr1 (</>) . L.dropWhileEnd (== drName) . splitDirectories . takeDirectory) drumkitFile



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


dgDefaultVersion :: Text
dgDefaultVersion = "2.0"
