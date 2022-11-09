{-# LANGUAGE DeriveGeneric, LambdaCase, NamedFieldPuns, OverloadedLabels, RankNTypes, ScopedTypeVariables #-}
module Logs.Impl (newActor) where

import Optics ((%))
import Optics.State.Operators ((%=), (.=))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import qualified Optics
import qualified Control.Monad.Trans.State as State
import qualified Data.Sequence as Seq

import Actor
import Logs.Msg
import qualified GUI.Msg as GUI


data ActorState = ActorState
  { entries
      :: Seq Entry
  , cursor
      :: Int
  }
  deriving Generic

newActor
  :: SendFunction (GUI.Msg Key)
  -> IO (Actor Msg)
newActor sendGui = do
  -- Append at the very end, regardless of the cursor's current position
  let appendEntry :: Entry -> StateT ActorState IO ()
      appendEntry entry = do
        c <- State.gets cursor
        n <- State.gets (Seq.length . entries)
        #entries %= (Seq.|> entry)
        if c == n
          then do
            #cursor %= succ

            liftIO $ sendGui $ GUI.InsertEntry () n $ GUI.Entry
              { GUI.entryText
                  = entry
              , GUI.entryGreyedOut
                  = False
              }
          else do
            liftIO $ sendGui $ GUI.InsertEntry () n $ GUI.Entry
              { GUI.entryText
                  = entry
              , GUI.entryGreyedOut
                  = True
              }

  -- True if the cursor did move
  let prev :: StateT ActorState IO Bool
      prev = do
        c <- State.gets cursor
        if c > 0
          then do
            let c' = c - 1
            #cursor .= c'

            Just entry <- State.gets $ Optics.preview (#entries % Optics.ix c')
            liftIO $ sendGui $ GUI.SetEntry () c' $ GUI.Entry
              { GUI.entryText
                  = entry
              , GUI.entryGreyedOut
                  = True
              }

            pure True
          else do
            pure False

  -- True if the cursor did move
  let next :: StateT ActorState IO Bool
      next = do
        c <- State.gets cursor
        n <- State.gets (Seq.length . entries)
        if c < n
          then do
            #cursor %= succ

            Just entry <- State.gets $ Optics.preview (#entries % Optics.ix c)
            liftIO $ sendGui $ GUI.SetEntry () c $ GUI.Entry
              { GUI.entryText
                  = entry
              , GUI.entryGreyedOut
                  = False
              }

            pure True
          else do
            pure False

  let actor :: Actor Msg
      actor = statefulActor (ActorState Seq.empty 0) $ \case
        AppendEntry entry request -> do
          appendEntry entry
          liftIO $ respond request ()
        Prev request -> do
          r <- prev
          liftIO $ respond request r
        Next request -> do
          r <- next
          liftIO $ respond request r
  pure actor
