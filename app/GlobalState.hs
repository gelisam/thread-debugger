{-# LANGUAGE DeriveGeneric #-}
module GlobalState where

import Data.Map (Map)
import GHC.Generics (Generic)
import qualified GI.Gtk as Gtk

import History
import Zipper


data Widgets = Widgets
  { widgetsWindow
      :: Gtk.Window
  , widgetsListBox
      :: Gtk.ListBox
  }
  deriving Generic

data RenderedEntry = RenderedEntry
  { renderedEntryItself
      :: Entry
  , renderedEntryLabel
      :: Gtk.Label
  }
  deriving Generic

data GlobalState = GlobalState
  { globalStateMainWindow
      :: Widgets
  , globalStateThreadWindows
      :: Map String Widgets
  , globalStateRenderedEntries
      :: Zipper RenderedEntry
  }
  deriving Generic
