module Logs.Msg where

import Actor


type Key = ()

type Entry = String

data Msg
  = AppendEntry
      Entry (Request ())
    -- ^ Append at the very end, regardless of the cursor's current position
  | Prev
      (Request Bool)
    -- ^ True if the cursor did move
  | Next
      (Request Bool)
    -- ^ True if the cursor did move
