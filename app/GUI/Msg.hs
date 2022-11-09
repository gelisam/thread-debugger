{-# LANGUAGE DeriveGeneric #-}
module GUI.Msg where

import GHC.Generics (Generic)

import Actor


data Entry = Entry
  { entryText
      :: String
  , entryGreyedOut
      :: Bool
  }
  deriving Generic

type RunApp
  = IO ()

data Msg k
  = InsertEntry
      k Int Entry (Request ())
    -- ^ (-1) or current count or bigger to append at the end
  | SetEntry
      k Int Entry (Request ())
    -- ^ noop if there is no entry at that position
  | RemoveEntry
      k Int (Request ())
    -- ^ noop if there is no entry at that position
  | QuitApp (Request ())
    -- ^ return control to RunApp's caller
