module Zipper where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty


data Zipper a = Zipper [a] a [a]

newZipperL
  :: NonEmpty a
  -> Zipper a
newZipperL (x :| xs)
  = Zipper [] x xs

-- focus on the last element
newZipperR
  :: NonEmpty a
  -> Zipper a
newZipperR xs0
  = let x :| xs = NonEmpty.reverse xs0
 in Zipper xs x []
