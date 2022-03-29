module Indexable where

import Data.List.NonEmpty (NonEmpty, (!!))
import Prelude hiding ((!!))

data IndexableError index
  = NegativeIndex index
  | IndexTooLarge index
  deriving (Show)

class Indexable i index | i -> index where
  index :: i a -> index -> Either (IndexableError index) a

instance Indexable NonEmpty Int where
  index nel i
    | i < 0 = Left (NegativeIndex i)
    | i >= length nel = Left (IndexTooLarge i)
    | otherwise = Right (nel !! i)