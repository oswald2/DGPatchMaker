{-# LANGUAGE
  OverloadedStrings
  , TypeApplications
#-}
module Gtk.DirectedChokeDialog
  ( DirectedChokeDialog
  , initDialog
  , showChokeDialog
  )
where

import           Data.Text
import           Graphics.UI.Gtk               as G
import           Control.Monad
import           Data.Types
import           Gtk.Utils



data DirectedChokeDialog = DirectedChokeDialog {
  dcDialog :: Dialog
  , dcCloseButton :: Button
  , dcInstrument :: Entry
  , dcAddButton :: Button
  , dcRemoveButton :: Button
  , dcTvAvailableInstruments :: TreeView
  , dcTvAvailableInstrumentsModel :: ListStore Text
  , dcTvChokedInstruments :: TreeView
  , dcTvChokedInsturmentsModel :: ListStore ChokeData
  }


initDialog :: Builder -> IO DirectedChokeDialog
initDialog builder = do

  diag <- builderGetObject builder castToDialog ("chokeDialog" :: Text)

  closeButton <- dialogAddButton diag ("Close" :: Text) ResponseClose

  instr <- builderGetObject builder castToEntry ("entryChokeInstrument" :: Text)
  availableInstr <- builderGetObject builder
                                     castToTreeView
                                     ("treeViewAvailableInstruments" :: Text)
  chokedInstr <- builderGetObject builder
                                  castToTreeView
                                  ("treeViewInstrumentsToChoke" :: Text)

  addButton    <- builderGetObject builder castToButton ("buttonCAdd" :: Text)
  removeButton <- builderGetObject builder
                                   castToButton
                                   ("buttonCRemove" :: Text)

  availableInstrLM <- listStoreNew []
  chokedInstrLM    <- listStoreNew []

  initAvailableInstruments availableInstr availableInstrLM
  initChokedInstruments chokedInstr chokedInstrLM

  let gui = DirectedChokeDialog
        { dcDialog                      = diag
        , dcCloseButton                 = closeButton
        , dcInstrument                  = instr
        , dcAddButton                   = addButton
        , dcRemoveButton                = removeButton
        , dcTvAvailableInstruments      = availableInstr
        , dcTvAvailableInstrumentsModel = availableInstrLM
        , dcTvChokedInstruments         = chokedInstr
        , dcTvChokedInsturmentsModel    = chokedInstrLM
        }

  return gui


showChokeDialog :: DirectedChokeDialog -> ChannelMap -> [Text] -> IO ()
showChokeDialog diag instrument availableInstruments = do
  entrySetText (dcInstrument diag) (cmName instrument)
  setListStoreTo (dcTvAvailableInstrumentsModel diag) availableInstruments
  listStoreClear (dcTvChokedInsturmentsModel diag)


initAvailableInstruments :: TreeView -> ListStore Text -> IO ()
initAvailableInstruments tv lm = do
  treeViewSetModel tv (Just lm)

  col <- treeViewColumnNew
  treeViewColumnSetTitle @Text col "Instruments"
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True

  cellLayoutSetAttributes col renderer lm $ \i -> [cellText := i]

  void $ treeViewAppendColumn tv col

  sel <- treeViewGetSelection tv
  treeSelectionSetMode sel SelectionMultiple



initChokedInstruments :: TreeView -> ListStore ChokeData -> IO ()
initChokedInstruments tv lm = do
  treeViewSetModel tv (Just lm)

  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew

  treeViewColumnSetTitle @Text col1 "Instrument"
  treeViewColumnSetTitle @Text col1 "Choke Time"

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew

  cellLayoutPackStart col1 renderer1 True
  cellLayoutPackStart col2 renderer2 True

  cellLayoutSetAttributes col1 renderer1 lm
    $ \(ChokeData i _) -> [cellText := i]
  cellLayoutSetAttributes col2 renderer2 lm
    $ \(ChokeData _ t) -> [cellText := pack (show t)]

  void $ treeViewAppendColumn tv col1
  void $ treeViewAppendColumn tv col2

  sel <- treeViewGetSelection tv
  treeSelectionSetMode sel SelectionMultiple
