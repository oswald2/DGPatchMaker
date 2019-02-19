{-# LANGUAGE TemplateHaskell #-}
module Gtk.DGPatchMakerBuilder
(builderFileAsString)
where


import Data.FileEmbed


builderFileAsString :: String 
builderFileAsString = $(embedStringFile "DGPatchMaker.glade")
