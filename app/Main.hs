{-# LANGUAGE OverloadedLabels, RankNTypes #-}
module Main where

import Data.Foldable (for_)
import Data.GI.Base
import qualified GI.Gdk.Constants as Gdk
import qualified GI.Gtk as Gtk


import Actor
import qualified GUI.Impl as GUI
import qualified GUI.Msg as GUI
import qualified Logs.Impl as Logs
import qualified Logs.Msg as Logs


exampleInput
  :: [String]
exampleInput
  = [ "A hello"
    , "B foo"
    , "A world"
    , "B bar"
    , "B baz"
    ]

mkWindowTitle
  :: Logs.Key
  -> String
mkWindowTitle _
  = "main"

onWindowClosed
  :: SendFunction (GUI.Msg Logs.Key)
  -> Logs.Key
  -> Gtk.WidgetDestroyCallback
onWindowClosed sendGui _ = do
  sendGui GUI.QuitApp

onWindowKeyPress
  :: SendFunction (GUI.Msg Logs.Key)
  -> SendFunction Logs.Msg
  -> Logs.Key
  -> Gtk.WidgetKeyPressEventCallback
onWindowKeyPress sendGui sendLogs _ eventKey = do
  eventKeyval <- get eventKey #keyval
  case eventKeyval of
    Gdk.KEY_Escape -> do
      sendGui GUI.QuitApp
      pure True
    Gdk.KEY_Left -> do
      sendLogs Logs.Prev
    Gdk.KEY_Right -> do
      sendLogs Logs.Next
    _ -> do
      --print eventKeyval
      pure False

onEntryKeyPress
  :: SendFunction (GUI.Msg Logs.Key)
  -> Logs.Key
  -> Int
  -> Gtk.WidgetKeyPressEventCallback
onEntryKeyPress _ _ _ _ = do
  pure False

main :: IO ()
main = do
  withTwoStepActor $ \sendGui cc -> do
    logsActor <- Logs.newActor sendGui
    withActor logsActor $ \sendLogs -> do
      (runApp, guiActor) <- GUI.newActor
        mkWindowTitle
        (onWindowClosed sendGui)
        (onWindowKeyPress sendGui sendLogs)
        (onEntryKeyPress sendGui)
      cc guiActor $ do
        for_ exampleInput $ \entry -> do
          sendLogs $ Logs.AppendEntry entry
        runApp
