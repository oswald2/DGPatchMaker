{-# LANGUAGE OverloadedStrings #-}
module Data.Drumgizmo

where


import Data.Text as T
import Control.Exception
import Control.Monad 

import System.Directory


createDrumgizmoDirectories :: FilePath -> IO (Either Text ())
createDrumgizmoDirectories path = do
    catch (do
        e <- doesDirectoryExist path
        unless e $ createDirectory path

        -- let instPath = getInstrumentDir path
        -- e1 <- doesDirectoryExist instPath
        -- unless e1 $ createDirectory instPath

        return (Right ())
        )
        (\e -> do
                let err = show (e :: SomeException)
                return (Left (pack err))
        )


dgDefaultVersion :: Text
dgDefaultVersion = "2.0"
