{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedLabels, RecursiveDo, ScopedTypeVariables #-}
module Impl (newHandle) where

import Control.Concurrent.STM
import Data.Foldable (for_)
import Data.GI.Base
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.String (IsString(fromString))
import GHC.Generics (Generic)
import qualified GI.Gtk as Gtk
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Handle


type GlobalState k = Map k WindowState

data WindowState = WindowState
  { windowItself
      :: Gtk.Window
  , windowListBox
      :: Gtk.ListBox
  , windowLabels
      :: TVar (Seq Gtk.Label)
  }
  deriving Generic


newWindowState
  :: String
       -- ^ window title
  -> Gtk.WidgetDestroyCallback
       -- ^ window closed callback
  -> Gtk.WidgetKeyPressEventCallback
       -- ^ window keypress callback
  -> IO WindowState
newWindowState windowTitle onWindowClosed onWindowKeyPress = do
  window <- new Gtk.Window [ #title := fromString windowTitle ]
  _ <- on window #destroy onWindowClosed
  _ <- on window #keyPressEvent onWindowKeyPress

  scrolledWindow <- new Gtk.ScrolledWindow []
  #add window scrolledWindow

  listBox <- new Gtk.ListBox []
  #add scrolledWindow listBox

  #showAll window

  labelsTVar <- newTVarIO Seq.empty

  pure $ WindowState
    { windowItself
        = window
    , windowListBox
        = listBox
    , windowLabels
        = labelsTVar
    }

getOrCreateWindowState
  :: forall k. Ord k
  => (k -> String)
       -- ^ window title
  -> (k -> Gtk.WidgetDestroyCallback)
       -- ^ window closed callback
  -> (k -> Gtk.WidgetKeyPressEventCallback)
       -- ^ window keypress callback
  -> TVar (GlobalState k)
  -> k
  -> IO WindowState
getOrCreateWindowState mkWindowTitle onWindowClosed onWindowKeyPress globalTVar k = do
  globalState <- atomically $ readTVar globalTVar
  case Map.lookup k globalState of
    Just windowState -> do
      pure windowState
    Nothing -> do
      windowState <- newWindowState
        (mkWindowTitle k)
        (onWindowClosed k)
        (onWindowKeyPress k)
      atomically $ do
        modifyTVar globalTVar $ Map.insert k windowState
      pure windowState

newHandle
  :: forall k. Ord k
  => (k -> String)
       -- ^ window title
  -> (Handle k -> k -> Gtk.WidgetDestroyCallback)
       -- ^ window closed callback
  -> (Handle k -> k -> Gtk.WidgetKeyPressEventCallback)
       -- ^ window keypress callback
  -> (Handle k -> k -> Int -> Gtk.WidgetKeyPressEventCallback)
       -- ^ entry keypress callback
  -> IO (Handle k)
newHandle mkWindowTitle onWindowClosed onWindowKeyPress onEntryKeyPress = mdo
  _ <- Gtk.init Nothing

  globalTVar <- newTVarIO Map.empty
  let getOrCreateWindowState' :: k -> IO WindowState
      getOrCreateWindowState'
        = getOrCreateWindowState
            mkWindowTitle
            (onWindowClosed handle)  -- tie the knot
            (onWindowKeyPress handle)  -- tie the knot
            globalTVar

  -- (-1) or current count or bigger to append at the end
  let insertEntry :: k -> Int -> Entry -> IO ()
      insertEntry k i entry = do
        WindowState
          { windowListBox = listBox
          , windowLabels = labelsTVar
          } <- getOrCreateWindowState' k
        n <- atomically $ length <$> readTVar labelsTVar
        let s = entryText entry
        let g = entryGreyedOut entry
        let i' = if i < 0 then n else i

        label <- new Gtk.Label
          [ #label := fromString s
          , #sensitive := not g
          ]
        _ <- on label #keyPressEvent (onEntryKeyPress handle k i)
        Gtk.listBoxInsert listBox label (fromIntegral i)
        #show label

        atomically $ modifyTVar labelsTVar $ Seq.insertAt i' label

  -- noop if there is no entry at that position
  let setEntry :: k -> Int -> Entry -> IO ()
      setEntry k i entry = do
        globalState <- atomically $ readTVar globalTVar
        for_ (Map.lookup k globalState) $ \(WindowState
                                              { windowLabels = labelsTVar
                                              }) -> do
          labels <- atomically $ readTVar labelsTVar
          for_ (Seq.lookup i labels) $ \label -> do
            let s = entryText entry
            let g = entryGreyedOut entry
            set label
              [ #label := fromString s
              , #sensitive := not g
              ]

  -- noop if there is no entry at that position
  let removeEntry :: k -> Int -> IO ()
      removeEntry k i = do
        globalState <- atomically $ readTVar globalTVar
        for_ (Map.lookup k globalState) $ \(WindowState
                                              { windowListBox = listBox
                                              , windowLabels = labelsTVar
                                              }) -> do
          labels <- atomically $ readTVar labelsTVar
          for_ (Seq.lookup i labels) $ \label -> do
            Gtk.containerRemove listBox label
            atomically $ modifyTVar labelsTVar $ Seq.deleteAt i

  let handle :: Handle k
      handle = Handle
        { insertEntry
        , setEntry
        , removeEntry
        , runApp = Gtk.main
        , quitApp = Gtk.mainQuit
        }
  pure handle
