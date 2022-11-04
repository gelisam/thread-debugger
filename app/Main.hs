{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedLabels, OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Concurrent.MVar
import Data.Foldable (for_)
import Data.GI.Base
import Data.Map (Map)
import Data.String (IsString(fromString))
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import qualified Control.Monad.Trans.State as State
import qualified GI.Gdk.Constants as Gdk
import qualified GI.Gtk as Gtk
import qualified Data.Map as Map


exampleInput
  :: [String]
exampleInput
  = [ "A hello"
    , "B foo"
    , "A world"
    , "B bar"
    , "B baz"
    ]

type Key
  = Maybe String

type Entry
  = (Key, String)

type History
  = [Entry]

parseEntry
  :: String
  -> Entry
parseEntry s
  = case words s of
      x1:x2:xs
        -> (Just x1, unwords (x2:xs))
      _
        -> (Nothing, s)

parseHistory
  :: [String]
  -> History
parseHistory
  = map parseEntry

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

data Zipper a = Zipper [a] a [a]

appendZipper
  :: a
  -> Zipper a
  -> Zipper a
appendZipper new (Zipper xs y zs)
  = Zipper (y:xs) new zs

data GlobalState = GlobalState
  { globalStateMainWindow
      :: Widgets
  , globalStateThreadWindows
      :: Map String Widgets
  , globalStateRenderedEntries
      :: Zipper RenderedEntry
  }
  deriving Generic

getListBox
  :: MVar GlobalState
  -> Key
  -> IO Gtk.ListBox
getListBox mvar k = do
  withMVar mvar $ \(GlobalState {..}) -> do
    pure $ widgetsListBox $ globalStateMainWindow

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

newGlobalState :: Widgets -> [RenderedEntry] -> IO (MVar GlobalState)
newGlobalState mainWindowWidgets renderedEntries = do
  let lastEntry : reversedEntries = reverse renderedEntries

  newMVar $ GlobalState
    { globalStateMainWindow
        = mainWindowWidgets
    , globalStateThreadWindows
        = Map.empty
    , globalStateRenderedEntries
        = Zipper reversedEntries lastEntry []
    }

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  
  mainWindow <- new Gtk.Window [ #title := "main" ]
  _ <- on mainWindow #destroy Gtk.mainQuit
  _ <- on mainWindow #keyPressEvent mainWindowKeyPressCallback

  scrolledWindow <- new Gtk.ScrolledWindow []
  #add mainWindow scrolledWindow

  listBox <- new Gtk.ListBox []
  #add scrolledWindow listBox

  renderedEntries <- for exampleInput $ \s -> do
    label <- new Gtk.Label [ #label := fromString s ]
    Gtk.listBoxInsert listBox label (-1)
    pure $ RenderedEntry
      { renderedEntryItself
          = (Nothing, s)
      , renderedEntryLabel
          = label
      }

  let mainWindowWidgets :: Widgets
      mainWindowWidgets = Widgets
        { widgetsWindow
            = mainWindow
        , widgetsListBox
            = listBox
        }
  mvar <- newGlobalState mainWindowWidgets renderedEntries
  #showAll mainWindow

  Gtk.main
