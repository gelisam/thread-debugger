{-# LANGUAGE OverloadedLabels, RankNTypes #-}
module Main where

import Control.Concurrent.STM
import Data.Foldable (for_)
import Data.GI.Base
import qualified GI.Gdk.Constants as Gdk
import qualified GI.Gtk as Gtk


import Actor
import Impl
import Msg


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
  :: SendFunction (Msg Key) -> Key -> Gtk.WidgetDestroyCallback
onWindowClosed send _ = do
  send QuitApp

onWindowKeyPress
  :: TVar Int -> SendFunction (Msg Key) -> Key -> Gtk.WidgetKeyPressEventCallback
onWindowKeyPress timeTVar send k eventKey = do
  eventKeyval <- get eventKey #keyval
  case eventKeyval of
    Gdk.KEY_Escape -> do
      send QuitApp
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
          send $ SetEntry k time' entry
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
          send $ SetEntry k time entry
          atomically $ writeTVar timeTVar time'
          pure True
        else do
          pure False
    _ -> do
      --print eventKeyval
      pure False

onEntryKeyPress
  :: SendFunction (Msg Key) -> Key -> Int -> Gtk.WidgetKeyPressEventCallback
onEntryKeyPress _ _ _ _ = do
  pure False

main :: IO ()
main = do
  timeTVar <- newTVarIO (length exampleInput)

  withTwoStepActor $ \send -> do
    (runApp, actor) <- newActor
      mkWindowTitle
      (onWindowClosed send)
      (onWindowKeyPress timeTVar send)
      (onEntryKeyPress send)
    pure $ (,) actor $ do
      for_ (zip [0..] exampleInput) $ \(i, s) -> do
        let entry = Entry
              { entryText
                  = s
              , entryGreyedOut
                  = False
              }
        send $ InsertEntry () i entry
      runApp
