{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Actor
  ( Request
  , Response
  , respond
  , Actor
  , statefulActor
  , statelessActor
  , SendFunction
  , withActor
  , withTwoStepActor
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.Trans.State
import Data.Void


-- e.g.
-- data MyMsg
--   = Add Int (Request ())
--   | GetCount (Request Int)

newtype Request a = Request (MVar a)

data Response = Response

respond :: Request a -> a -> IO Response
respond (Request mvar) a = do
  putMVar mvar a
  pure Response

newtype Actor msg = Actor
  { unActor :: msg -> IO (Response, Actor msg)
  }

statefulActor
  :: s
  -> (msg -> StateT s IO Response)
  -> Actor msg
statefulActor s0 f = Actor $ \msg -> do
  (response, s1) <- runStateT (f msg) s0
  pure $ (response, statefulActor s1 f)

statelessActor
  :: (msg -> IO Response)
  -> Actor msg
statelessActor f = Actor $ \msg -> do
  response <- f msg
  pure (response, statelessActor f)

-- blocks until the actor replies, throwing an exception if the Actor terminates early.
type SendFunction msg = forall a. (Request a -> msg) -> IO a

-- spawn the actor, providing the body with a way to send messages to it. the
-- actor is stopped once the body ends.
withActor
  :: forall msg r
   . Actor msg
  -> (SendFunction msg -> IO r)
  -> IO r
withActor actor0 body = do
  let mkActorAndBody :: SendFunction msg -> IO (Actor msg, IO r)
      mkActorAndBody sendFunction = do
        pure (actor0, body sendFunction)
  withTwoStepActor mkActorAndBody

-- a variant of 'withActor' which gives you the 'SendFunction' early, so you
-- can construct actors which call each other.
withTwoStepActor
  :: forall msg r
   . (SendFunction msg -> IO (Actor msg, IO r))
  -> IO r
withTwoStepActor mkActorAndBody = do
  msgMVar <- newEmptyMVar
  actorMVar <- newEmptyMVar
  let runActor :: Actor msg -> IO Void
      runActor actor = do
        msg <- takeMVar msgMVar
        (Response, actor') <- unActor actor msg
        runActor actor'
  let sendFunction :: forall a. (Request a -> msg) -> IO a
      sendFunction mkMsg = do
        responseMVar <- newEmptyMVar
        let msg = mkMsg (Request responseMVar)
        putMVar msgMVar msg
        actorAsync <- readMVar actorMVar
        r <- race
          (takeMVar responseMVar)
          (wait actorAsync)
        case r of
          Left a -> do
            pure a
          Right void -> do
            absurd void
  (actor0, body) <- mkActorAndBody sendFunction
  withAsync (runActor actor0) $ \actorAsync -> do
    putMVar actorMVar actorAsync
    body
