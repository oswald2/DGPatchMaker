{-# LANGUAGE
  OverloadedStrings
  , TypeApplications
  , BangPatterns
#-}
module Gtk.DirectedChokeDialog
  ( DirectedChokeDialog
  , initDialog
  , showChokeDialog
  )
where

import           Graphics.UI.Gtk               as G
import           Control.Monad
import           Data.Types
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Text.Parsec
import           Text.ParserCombinators.Parsec.Number

import           Gtk.Utils



data DirectedChokeDialog = DirectedChokeDialog {
  dcWindow :: Window
  , dcDialog :: Dialog
  , dcCloseButton :: Button
  , dcOkButton :: Button
  , dcInstrument :: Entry
  , dcAddButton :: Button
  , dcRemoveButton :: Button
  , dcTvAvailableInstruments :: TreeView
  , dcTvAvailableInstrumentsModel :: ListStore Text
  , dcTvChokedInstruments :: TreeView
  , dcTvChokedInsturmentsModel :: ListStore ChokeData
  , dcRef :: IORef (Maybe ChannelMap)
  }


initDialog :: Window -> Builder -> IO DirectedChokeDialog
initDialog window builder = do

  diag <- builderGetObject builder castToDialog ("chokeDialog" :: Text)

  closeButton <- dialogAddButton diag ("Cancel" :: Text) ResponseClose
  okButton <- dialogAddButton diag ("OK" :: Text) ResponseOk

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
  initChokedInstruments window chokedInstr chokedInstrLM

  ref <- newIORef Nothing

  let gui = DirectedChokeDialog
        { dcDialog                      = diag
        , dcCloseButton                 = closeButton
        , dcOkButton                    = okButton
        , dcInstrument                  = instr
        , dcAddButton                   = addButton
        , dcRemoveButton                = removeButton
        , dcTvAvailableInstruments      = availableInstr
        , dcTvAvailableInstrumentsModel = availableInstrLM
        , dcTvChokedInstruments         = chokedInstr
        , dcTvChokedInsturmentsModel    = chokedInstrLM
        , dcRef                         = ref
        , dcWindow                      = window
        }


  void $ on addButton buttonActivated $ addInstrument gui
  void $ on removeButton buttonActivated $ removeInstrument gui

  return gui


showChokeDialog
  :: DirectedChokeDialog -> ChannelMap -> [Text] -> IO (Maybe ChannelMap)
showChokeDialog diag instrument availableInstruments = do
  writeIORef (dcRef diag) (Just instrument)
  entrySetText (dcInstrument diag) (cmName instrument)

  let chokes = map chokeInstrument (getChokes instrument)
      newAvailableInstruments = filter (`notElem` chokes) availableInstruments

  setListStoreTo (dcTvAvailableInstrumentsModel diag) newAvailableInstruments
  setListStoreTo (dcTvChokedInsturmentsModel diag) (getChokes instrument)

  res <- dialogRun (dcDialog diag)
  widgetHide (dcDialog diag)
  case res of
    ResponseOk -> getNewChannelMap diag
    _          -> return Nothing


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



initChokedInstruments :: Window -> TreeView -> ListStore ChokeData -> IO ()
initChokedInstruments window tv lm = do
  treeViewSetModel tv (Just lm)

  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew

  treeViewColumnSetTitle @Text col1 "Instrument"
  treeViewColumnSetTitle @Text col2 "Choke Time"

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew

  set renderer2 [cellTextEditable := True, cellTextEditableSet := True]

  cellLayoutPackStart col1 renderer1 True
  cellLayoutPackStart col2 renderer2 True

  cellLayoutSetAttributes col1 renderer1 lm
    $ \(ChokeData i _) -> [cellText := i]
  cellLayoutSetAttributes col2 renderer2 lm
    $ \(ChokeData _ t) -> [cellText := T.pack (show t)]

  void $ treeViewAppendColumn tv col1
  void $ treeViewAppendColumn tv col2

  sel <- treeViewGetSelection tv
  treeSelectionSetMode sel SelectionMultiple

  -- edit call back for editing the channels
  void $ G.on renderer2 edited $ \[i] str -> do
    oldVal <- listStoreGetValue lm i
    if null str 
      then do
        let newVal = oldVal { chokeTime = Nothing }
        listStoreSetValue lm i newVal
      else do 
        case parse int "" str of
          Left err ->
            displayErrorBox window
              $  "Error: time must be an integer number: "
              <> T.pack (show err)
          Right t -> do
            let newVal = oldVal { chokeTime = Just t }
            listStoreSetValue lm i newVal




getNewChannelMap :: DirectedChokeDialog -> IO (Maybe ChannelMap)
getNewChannelMap diag = do
  inst' <- readIORef (dcRef diag)
  case inst' of
    Nothing   -> return Nothing
    Just inst -> do
      lst <- listStoreToList (dcTvChokedInsturmentsModel diag)
      let newChokes = if Prelude.null lst then Disabled [] else Enabled lst
          !newCm    = inst { cmChokes = newChokes }
      return (Just newCm)



addInstrument :: DirectedChokeDialog -> IO ()
addInstrument diag = do
  sel         <- treeViewGetSelection (dcTvAvailableInstruments diag)
  rows        <- Prelude.map Prelude.head <$> treeSelectionGetSelectedRows sel

  instruments <- forM rows
    $ listStoreGetValue (dcTvAvailableInstrumentsModel diag)
  forM_ instruments $ \inst ->
    listStoreAppend (dcTvChokedInsturmentsModel diag) (ChokeData inst Nothing)

  -- now remove the instruments
  insts <- listStoreToList (dcTvAvailableInstrumentsModel diag)
  let newInsts = filter (`notElem` instruments) insts
  setListStoreTo (dcTvAvailableInstrumentsModel diag) newInsts


removeInstrument :: DirectedChokeDialog -> IO ()
removeInstrument diag = do
  sel         <- treeViewGetSelection (dcTvChokedInstruments diag)
  rows        <- Prelude.map Prelude.head <$> treeSelectionGetSelectedRows sel

  chokes <- forM rows
    $ listStoreGetValue (dcTvChokedInsturmentsModel diag)
  forM_ chokes $ \inst ->
    listStoreAppend (dcTvAvailableInstrumentsModel diag) (chokeInstrument inst)

  -- now remove the instruments
  oldChokes <- listStoreToList (dcTvChokedInsturmentsModel diag)
  let newChokes = filter (`notElem` chokes) oldChokes 
  setListStoreTo (dcTvChokedInsturmentsModel diag) newChokes

