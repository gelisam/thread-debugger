{-# LANGUAGE OverloadedLabels #-}
module Main where

import Control.Concurrent.STM
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
  :: TVar Int -> Handle Key -> Key -> Gtk.WidgetKeyPressEventCallback
onWindowKeyPress timeTVar handle k eventKey = do
  eventKeyval <- get eventKey #keyval
  case eventKeyval of
    Gdk.KEY_Escape -> do
      quitApp handle
      pure True
    Gdk.KEY_Left -> do
      time <- atomically $ readTVar timeTVar
      if time > 0
        then do
          let time' = time - 1
          let entry = Entry
                { entryText
                    = exampleInput !! time'
                , entryGreyedOut
                    = True
                }
          setEntry handle k time' entry
          atomically $ writeTVar timeTVar time'
          pure True
        else do
          pure False
    Gdk.KEY_Right -> do
      time <- atomically $ readTVar timeTVar
      let n = length exampleInput
      if time < n
        then do
          let time' = time + 1
          let entry = Entry
                { entryText
                    = exampleInput !! time
                , entryGreyedOut
                    = False
                }
          setEntry handle k time entry
          atomically $ writeTVar timeTVar time'
          pure True
        else do
          pure False
    _ -> do
      --print eventKeyval
      pure False

onEntryKeyPress
  :: Handle Key -> Key -> Int -> Gtk.WidgetKeyPressEventCallback
onEntryKeyPress _ _ _ _ = do
  pure False

main :: IO ()
main = do
  timeTVar <- newTVarIO (length exampleInput)

  handle <- newHandle
    mkWindowTitle
    onWindowClosed
    (onWindowKeyPress timeTVar)
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
