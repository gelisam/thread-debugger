{-# LANGUAGE DeriveGeneric #-}
module Handle where

import GHC.Generics (Generic)


data Entry = Entry
  { entryText
      :: String
  , entryGreyedOut
      :: Bool
  }
  deriving Generic

data Handle k = Handle
  { insertEntry
      :: k -> Int -> Entry -> IO ()
    -- ^ (-1) or current count or bigger to append at the end
  , setEntry
      :: k -> Int -> Entry -> IO ()
    -- ^ noop if there is no entry at that position
  , removeEntry
      :: k -> Int -> IO ()
    -- ^ noop if there is no entry at that position
  , runApp
      :: IO ()
    -- ^ give control to the Handle until 'quitApp'
  , quitApp
      :: IO ()
    -- ^ return control to the caller of 'runApp'
  }
