{-# LANGUAGE LambdaCase, OverloadedLabels, OverloadedStrings, RecordWildCards #-}
module Main where

import Data.Foldable (for_)
import Data.GI.Base
import Data.Map (Map)
import Data.String (IsString(fromString))
import qualified Control.Monad.Trans.State as State
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

type Entry
  = (Maybe String, String)

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
  , widgetsButton
      :: Gtk.Button
  }

type Windows
  = Map (Maybe String) Widgets

mainWindowKeyPressCallback :: Gtk.WidgetKeyPressEventCallback
mainWindowKeyPressCallback eventKey = do
  eventString <- get eventKey #string
  case eventString of
    Just "\ESC" -> do
      Gtk.mainQuit
      pure True
    _ -> do
      pure False

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

  for_ exampleInput $ \s -> do
    label <- new Gtk.Label [ #label := fromString s ]
    Gtk.listBoxInsert listBox label (-1)
  #showAll mainWindow
