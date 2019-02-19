{-# LANGUAGE TemplateHaskell #-}
module Gtk.InstrumentPageBuilder
(builderFileAsString)
where


import Data.FileEmbed


builderFileAsString :: String
builderFileAsString = $(embedStringFile "InstrumentPage.glade")
