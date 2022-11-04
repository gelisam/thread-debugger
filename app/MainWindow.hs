{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
module MainWindow where

import Control.Concurrent.MVar
import Data.GI.Base
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString(fromString))
import Data.Traversable (for)
import qualified GI.Gdk.Constants as Gdk
import qualified GI.Gtk as Gtk
import qualified Data.Map as Map

import GlobalState
import Zipper


mainWindowKeyPressCallback :: Gtk.WidgetKeyPressEventCallback
mainWindowKeyPressCallback eventKey = do
  eventKeyval <- get eventKey #keyval
  case eventKeyval of
    Gdk.KEY_Escape -> do
      Gtk.mainQuit
      pure True
    Gdk.KEY_Left -> do
      putStrLn "left"
      pure True
    _ -> do
      --print eventKeyval
      pure False

newMainWindow
  :: MVar GlobalState
  -> NonEmpty String
  -> IO GlobalState
newMainWindow _mvar ss = do
  mainWindow <- new Gtk.Window [ #title := "main" ]
  _ <- on mainWindow #destroy Gtk.mainQuit
  _ <- on mainWindow #keyPressEvent mainWindowKeyPressCallback

  scrolledWindow <- new Gtk.ScrolledWindow []
  #add mainWindow scrolledWindow

  listBox <- new Gtk.ListBox []
  #add scrolledWindow listBox
  
  let mainWindowWidgets = Widgets
        { widgetsWindow
            = mainWindow
        , widgetsListBox
            = listBox
        }

  renderedEntries <- for ss $ \s -> do
    label <- new Gtk.Label [ #label := fromString s ]
    Gtk.listBoxInsert listBox label (-1)
    pure $ RenderedEntry
      { renderedEntryItself
          = (Nothing, s)
      , renderedEntryLabel
          = label
      }

  #showAll mainWindow

  pure $ GlobalState
    { globalStateMainWindow
       = mainWindowWidgets
    , globalStateThreadWindows
        = Map.empty
    , globalStateRenderedEntries
        = newZipperR renderedEntries
    }
