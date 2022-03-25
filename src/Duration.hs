module Duration (Duration, DurationError (NegativeValue), create, mean) where

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as Time
import qualified GHC.Read as Read

newtype Duration = Duration NominalDiffTime deriving (Eq)

newtype DurationError = NegativeValue NominalDiffTime deriving (Show)

create :: NominalDiffTime -> Either DurationError Duration
create duration =
  if duration < 0
    then Left (NegativeValue duration)
    else Right (Duration duration)

mean :: NonEmpty Duration -> Duration
mean durations =
  sum durations / realToFrac (length durations)

-- Instances

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