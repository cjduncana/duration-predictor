-- |
-- Module: Duration
-- Description: The amount of time elapsed between two events
--
-- A 'Duration' is the amount of time elapsed between two events.
module Domain.Duration (Duration, DurationError (NegativeValue), create, mean) where

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as Time
import qualified GHC.Read as Read

-- | Amount of time elapsed
newtype Duration = Duration NominalDiffTime deriving (Eq)

-- | Possible reasons why this module will not create a 'Duration'
newtype DurationError
  = NegativeValue   -- ^ This module guarantees that all 'Duration' are
                    -- non-negative
    NominalDiffTime -- ^ An invalid 'NominalDiffTime'
    deriving (Show)

-- | Smart constructor for 'Duration'
--
-- Provide a non-negative 'NominalDiffTime' to create a 'Duration'
create  :: NominalDiffTime -- ^ Amount of time elapsed
        -> Either DurationError Duration
create duration =
  if duration < 0
    then Left (NegativeValue duration)
    else Right (Duration duration)

-- | The arithmetic mean of more than one 'Duration'
mean :: NonEmpty Duration -> Duration
mean durations =
  sum durations / realToFrac (length durations)

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
  fromInteger = Duration . fromInteger

instance Fractional Duration where
  fromRational = Duration . fromRational
  recip (Duration duration) = Duration (recip duration)

instance Show Duration where
  show (Duration duration) = show duration

instance Read NominalDiffTime where
  readPrec = Time.secondsToNominalDiffTime <$> Read.readPrec