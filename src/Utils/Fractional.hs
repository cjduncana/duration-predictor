module Utils.Fractional where

nextAverage :: Fractional a => a -> a -> a -> a
nextAverage count previousAverage nextValue =
  ((count * previousAverage) + nextValue) / (count + 1)