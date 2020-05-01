{-# LANGUAGE 
  OverloadedStrings
  , BangPatterns
  , TypeApplications
#-}
module Gtk.ClickyKitDialog
  ( ClickyKitDialog
  , initClickyKitDialog
  , showClickyKitDialog
  )
where


import           Graphics.UI.Gtk               as G
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Types
import           Data.IORef
import           Data.Word
import           Data.Bits
import           Data.Functor.Identity
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Gtk.Utils

import           Text.Parsec
import           System.FilePath
import           Data.Array.MArray
import           Text.Builder                  as TB


data ClickyKitDialog = ClickyKitDialog {
  ckdWindow :: Window
  , ckdDialog :: Dialog
  , ckdTreeView :: TreeView
  , ckdTreeModel :: ListStore ClickMapItem
  , ckdDKButton :: Button
  , ckdDKEntry :: Entry
  , ckdDKImage :: Image
  , ckdCMButton :: Button
  , ckdCMEntry :: Entry
  , ckdCMImage :: Image
  , ckdCMEventBox :: EventBox
  , ckdRef :: IORef (Maybe ImageData)
  }


initClickyKitDialog :: Window -> G.Builder -> IO ClickyKitDialog
initClickyKitDialog window builder = do

  diag <- builderGetObject builder castToDialog ("clickyKitDialog" :: Text)

  void $ dialogAddButton diag ("Cancel" :: Text) ResponseClose
  void $ dialogAddButton diag ("OK" :: Text) ResponseOk

  tv <- builderGetObject builder
                         castToTreeView
                         ("treeviewCKInstruments" :: Text)

  dkButton <- builderGetObject builder
                               castToButton
                               ("buttonCKBrowseImage" :: Text)
  dkEntry <- builderGetObject builder
                              castToEntry
                              ("entryCKDrumImageFile" :: Text)
  dkImage  <- builderGetObject builder castToImage ("imageDrumKit" :: Text)

  cmButton <- builderGetObject builder castToButton ("buttonCKClickMap" :: Text)
  cmEntry  <- builderGetObject builder
                               castToEntry
                               ("entryCKClickMapFile" :: Text)
  cmImage  <- builderGetObject builder castToImage ("imageClickMap" :: Text)
  eventBox <- builderGetObject builder castToEventBox ("eventBox" :: Text)


  widgetAddEvents cmImage [ButtonPressMask]

  model <- listStoreNew []
  ref   <- newIORef Nothing

  treeViewSetModel tv (Just model)

  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew

  treeViewColumnSetTitle @Text col1 "Instrument"
  treeViewColumnSetTitle @Text col2 "Colour"

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew

  set renderer2 [cellTextEditable := True, cellTextEditableSet := True]

  cellLayoutPackStart col1 renderer1 True
  cellLayoutPackStart col2 renderer2 True

  cellLayoutSetAttributes col1 renderer1 model
    $ \cmi -> [cellText := cmiInstrument cmi]
  cellLayoutSetAttributes col2 renderer2 model
    $ \cmi -> [cellText := cmiColour cmi]

  void $ treeViewAppendColumn tv col1
  void $ treeViewAppendColumn tv col2

  -- edit call back for editing the channels
  void $ G.on renderer2 edited $ \[i] str -> do
    oldVal <- listStoreGetValue model i
    if not (T.null str)
      then do
        case parse colour "" str of
          Left err ->
            displayErrorBox window
              $  "Error: not a valid colour: "
              <> str
              <> ": "
              <> T.pack (show err)
          Right t -> do
            let newVal = oldVal { cmiColour = t }
            listStoreSetValue model i newVal
      else do
        let newVal = oldVal { cmiColour = "" }
        listStoreSetValue model i newVal

  let gui = ClickyKitDialog { ckdWindow     = window
                            , ckdDialog     = diag
                            , ckdTreeView   = tv
                            , ckdTreeModel  = model
                            , ckdDKButton   = dkButton
                            , ckdDKEntry    = dkEntry
                            , ckdDKImage    = dkImage
                            , ckdCMButton   = cmButton
                            , ckdCMEntry    = cmEntry
                            , ckdCMImage    = cmImage
                            , ckdCMEventBox = eventBox
                            , ckdRef        = ref
                            }

  void $ on eventBox buttonPressEvent $ doubleClickCB gui

  return gui


doubleClickCB :: ClickyKitDialog -> EventM EButton Bool
doubleClickCB diag = do
  button <- eventButton
  case button of
    LeftButton -> do
      cl <- eventClick
      case cl of
        DoubleClick -> do
          px <- eventCoordinates
          color <- liftIO $ getColor diag px
          liftIO $ putStrLn $ "ClickMap clicked: " <> show px <> " color: " <> T.unpack color
          return True
        _ -> return False
    _ -> return False


getColor :: ClickyKitDialog -> (Double, Double) -> IO Text
getColor diag (x, y) = do
  pixbuf    <- imageGetPixbuf (ckdCMImage diag)
  rowstride <- pixbufGetRowstride pixbuf
  bps       <- pixbufGetBitsPerSample pixbuf
  nchan     <- pixbufGetNChannels pixbuf

  if bps /= 8
    then do
      displayErrorBox (ckdWindow diag)
        $  "Error: number of bits per colour channel is not supported ("
        <> run (decimal bps)
      return ""
    else do
      if nchan >= 3 
        then do
          pixels <- pixbufGetPixels pixbuf :: IO (PixbufData Int Word8)
          let idx = round y * rowstride + round x * nchan 
          r <- readArray pixels idx
          g <- readArray pixels (idx + 1)
          b <- readArray pixels (idx + 2)
          let val :: Word32
              !val =
                fromIntegral r
                  `shiftL` 16
                  .|.      fromIntegral g
                  `shiftL` 8
                  .|.      fromIntegral b
          return (run (hexadecimal val))
        else return ""


colour :: ParsecT Text u Identity Text
colour = T.pack <$> count 6 hexDigit

showClickyKitDialog
  :: ClickyKitDialog -> ImageData -> FilePath -> IO (Maybe ImageData)
showClickyKitDialog diag image basepath = do
  setImageData diag image basepath
  res <- dialogRun (ckdDialog diag)
  widgetHide (ckdDialog diag)
  case res of
    ResponseOk -> Just <$> getImageData diag
    _          -> return Nothing


setImageData :: ClickyKitDialog -> ImageData -> FilePath -> IO ()
setImageData diag img basepath = do
  writeIORef (ckdRef diag) (Just img)
  setListStoreTo (ckdTreeModel diag) (imgClickMap img)
  entrySetText (ckdDKEntry diag) (imgSource img)
  entrySetText (ckdCMEntry diag) (imgMap img)

  imageSetFromFile (ckdDKImage diag) (basepath </> T.unpack (imgSource img))
  imageSetFromFile (ckdCMImage diag) (basepath </> T.unpack (imgMap img))


getImageData :: ClickyKitDialog -> IO ImageData
getImageData diag = do
  ImageData
    <$> entryGetText (ckdDKEntry diag)
    <*> entryGetText (ckdCMEntry diag)
    <*> listStoreToList (ckdTreeModel diag)

