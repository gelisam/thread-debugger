{-# LANGUAGE OverloadedLabels #-}
module Main where

import Data.Foldable (for_)
import Data.GI.Base
import qualified GI.Gdk.Constants as Gdk
import qualified GI.Gtk as Gtk


import Handle
import Impl


exampleInput
  :: [String]
exampleInput
  = [ "A hello"
    , "B foo"
    , "A world"
    , "B bar"
    , "B baz"
    ]

type Key = ()

mkWindowTitle
  :: Key -> String
mkWindowTitle _
  = "main"

onWindowClosed
  :: Handle Key -> Key -> Gtk.WidgetDestroyCallback
onWindowClosed handle _ = do
  quitApp handle

onWindowKeyPress
  :: Handle Key -> Key -> Gtk.WidgetKeyPressEventCallback
onWindowKeyPress handle _ eventKey = do
  eventKeyval <- get eventKey #keyval
  case eventKeyval of
    Gdk.KEY_Escape -> do
      quitApp handle
      pure True
    _ -> do
      --print eventKeyval
      pure False

onEntryKeyPress
  :: Handle Key -> Key -> Int -> Gtk.WidgetKeyPressEventCallback
onEntryKeyPress _ _ _ _ = do
  pure False

main :: IO ()
main = do
  handle <- newHandle
    mkWindowTitle
    onWindowClosed
    onWindowKeyPress
    onEntryKeyPress

  for_ (zip [0..] exampleInput) $ \(i, s) -> do
    let entry = Entry
          { entryText
              = s
          , entryGreyedOut
              = False
          }
    insertEntry handle () i entry

  runApp handle
