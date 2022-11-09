{-# LANGUAGE DeriveGeneric, LambdaCase, NamedFieldPuns, OverloadedLabels, ScopedTypeVariables #-}
module GUI.Impl (newActor) where

import Optics ((%), ix)
import Optics.State.Operators ((%=))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT)
import Data.Foldable (for_)
import Data.GI.Base
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.String (IsString(fromString))
import GHC.Generics (Generic)
import qualified GI.Gtk as Gtk
import qualified Control.Monad.Trans.State as State
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Actor
import GUI.Msg


data WindowState = WindowState
  { windowItself
      :: Gtk.Window
  , windowListBox
      :: Gtk.ListBox
  , windowLabels
      :: Seq Gtk.Label
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

  pure $ WindowState
    { windowItself
        = window
    , windowListBox
        = listBox
    , windowLabels
        = Seq.empty
    }

getOrCreateWindowState
  :: forall k. Ord k
  => (k -> String)
       -- ^ window title
  -> (k -> Gtk.WidgetDestroyCallback)
       -- ^ window closed callback
  -> (k -> Gtk.WidgetKeyPressEventCallback)
       -- ^ window keypress callback
  -> k
  -> StateT (Map k WindowState) IO WindowState
getOrCreateWindowState mkWindowTitle onWindowClosed onWindowKeyPress k = do
  State.gets (Map.lookup k) >>= \case
    Just windowState -> do
      pure windowState
    Nothing -> do
      windowState <- liftIO $ newWindowState
        (mkWindowTitle k)
        (onWindowClosed k)
        (onWindowKeyPress k)
      State.modify $ Map.insert k windowState
      pure windowState

newActor
  :: forall k. Ord k
  => (k -> String)
       -- ^ window title
  -> (k -> Gtk.WidgetDestroyCallback)
       -- ^ window closed callback
  -> (k -> Gtk.WidgetKeyPressEventCallback)
       -- ^ window keypress callback
  -> (k -> Int -> Gtk.WidgetKeyPressEventCallback)
       -- ^ entry keypress callback
  -> IO (RunApp, Actor (Msg k))
newActor mkWindowTitle onWindowClosed onWindowKeyPress onEntryKeyPress = do
  _ <- Gtk.init Nothing

  let getOrCreateWindowState' :: k -> StateT (Map k WindowState) IO WindowState
      getOrCreateWindowState'
        = getOrCreateWindowState
            mkWindowTitle
            onWindowClosed
            onWindowKeyPress

  -- (-1) or current count or bigger to append at the end
  let insertEntry :: k -> Int -> Entry -> StateT (Map k WindowState) IO ()
      insertEntry k i entry = do
        WindowState
          { windowListBox = listBox
          , windowLabels = labels
          } <- getOrCreateWindowState' k
        let n = length labels
        let s = entryText entry
        let g = entryGreyedOut entry
        let i' = if i < 0 then n else i

        label <- new Gtk.Label
          [ #label := fromString s
          , #sensitive := not g
          ]
        _ <- on label #keyPressEvent (onEntryKeyPress k i)
        Gtk.listBoxInsert listBox label (fromIntegral i)
        #show label

        ix k % #windowLabels %= Seq.insertAt i' label

  -- noop if there is no entry at that position
  let setEntry :: k -> Int -> Entry -> StateT (Map k WindowState) IO ()
      setEntry k i entry = do
        maybeWindowState <- State.gets (Map.lookup k)
        for_ maybeWindowState $ \(WindowState
                                    { windowLabels = labels
                                    }) -> do
          for_ (Seq.lookup i labels) $ \label -> do
            let s = entryText entry
            let g = entryGreyedOut entry
            set label
              [ #label := fromString s
              , #sensitive := not g
              ]

  -- noop if there is no entry at that position
  let removeEntry :: k -> Int -> StateT (Map k WindowState) IO ()
      removeEntry k i = do
        maybeWindowState <- State.gets (Map.lookup k)
        for_ maybeWindowState $ \(WindowState
                                    { windowListBox = listBox
                                    , windowLabels = labels
                                    }) -> do
          for_ (Seq.lookup i labels) $ \label -> do
            Gtk.containerRemove listBox label
            ix k % #windowLabels %= Seq.deleteAt i

  let actor :: Actor (Msg k)
      actor = statefulActor Map.empty $ \case
        InsertEntry k i entry request -> do
          insertEntry k i entry
          liftIO $ respond request ()
        SetEntry k i entry request -> do
          setEntry k i entry
          liftIO $ respond request ()
        RemoveEntry k i request -> do
          removeEntry k i
          liftIO $ respond request ()
        QuitApp request -> do
          Gtk.mainQuit
          liftIO $ respond request ()
  pure (Gtk.main, actor)
