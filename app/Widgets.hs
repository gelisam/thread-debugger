{-# LANGUAGE RecordWildCards #-}
module Widgets where

import Control.Concurrent.MVar
import qualified GI.Gtk as Gtk

import History
import GlobalState


getListBox
  :: MVar GlobalState
  -> Key
  -> IO Gtk.ListBox
getListBox mvar _k = do
  withMVar mvar $ \(GlobalState {..}) -> do
    pure $ widgetsListBox $ globalStateMainWindow
