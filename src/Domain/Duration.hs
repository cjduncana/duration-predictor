-- |
-- Module: Duration
-- Description: The amount of time elapsed between two events
--
-- A 'Duration' is the amount of time elapsed between two events.
module Domain.Duration
  ( Duration,
    DurationError (NegativeValue),
    Domain.Duration.toInt,
    create,
    mean,
    nextAverage,
  )
where

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Ratio (Ratio, (%))
import qualified Data.Time.Clock as Time
import qualified GHC.Read as Read
import qualified Utils.Fractional as Fractional

-- | Amount of time elapsed in seconds
newtype Duration = Duration Int deriving (Eq)

-- | Possible reasons why this module will not create a 'Duration'
newtype DurationError
  = -- | This module guarantees that all 'Duration' are non-negative
    NegativeValue Int
  deriving (Show)

-- | Smart constructor for 'Duration'
--
-- Provide a non-negative 'Integer' to create a 'Duration'
create ::
  -- | Amount of time elapsed
  Int ->
  Either DurationError Duration
create duration =
  if duration < 0
    then Left (NegativeValue duration)
    else Right (Duration duration)

toInt :: Duration -> Int
toInt (Duration duration) = duration

-- | The arithmetic mean of more than one 'Duration'
mean :: NonEmpty Duration -> Ratio Int
mean durations =
  total % length durations
  where
    (Duration total) = sum durations

nextAverage :: Int -> Ratio Int -> Duration -> Ratio Int
nextAverage count previousAverage (Duration nextDuration) =
  Fractional.nextAverage
    (fromIntegral count)
    previousAverage
    (fromIntegral nextDuration)

-- Instances

-- | Addition is associative:
--
-- prop> (x + y) + z == x + (y + z)
--
-- Addition is commutative:
--
-- prop> x + y == y + x
--
-- 0 is an additive identity:
--
-- prop> x + 0 == x
--
-- All additive inverses are also additive identities:
--
-- prop> x + negate y == x
--
-- Multiplication is associative:
--
-- prop> (x * y) * z == x * (y * z)
--
-- 1 is the multiplicative identity:
--
-- prop> 1 * x == x
-- prop> x * 1 == x
--
-- Multiplication is distributive over addition:
--
-- prop> a * (b + c) == (a * b) + (a * c)
-- prop> (b + c) * a == (b * a) + (c * a)
instance Num Duration where
  (+) (Duration a) (Duration b) = Duration (a + b)
  (-) (Duration a) (Duration b) = if a > b then Duration (a - b) else 0
  (*) (Duration a) (Duration b) = Duration (a * b)
  abs = id
  signum (Duration duration) = Duration (signum duration)
  fromInteger = Duration . fromIntegral

instance Show Duration where
  show (Duration duration) = show duration ++ " seconds"