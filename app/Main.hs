{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Concurrent.MVar
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified GI.Gtk as Gtk


import MainWindow


exampleInput
  :: NonEmpty String
exampleInput
  = "A hello"
 :| "B foo"
  : "A world"
  : "B bar"
  : "B baz"
  : []

main :: IO ()
main = mdo
  _ <- Gtk.init Nothing
  
  globalState <- newMainWindow mvar exampleInput
  mvar <- newMVar globalState

  Gtk.main
