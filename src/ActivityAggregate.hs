module ActivityAggregate (ActivityAggregate, create, measure, predictDuration) where

import ActivityAggregate.Activity (Activity)
import qualified ActivityAggregate.Activity as Activity
import ActivityAggregate.Measurement (Measurement)
import qualified ActivityAggregate.Measurement as Measurement
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Duration (Duration)
import qualified Duration
import Entity (Entity (getId))
import NonEmptyString (NonEmptyString)

newtype ActivityAggregate = ActivityAggregate (Activity, NonEmpty Measurement)

create :: UUID -> NonEmptyString -> UUID -> Duration -> UTCTime -> ActivityAggregate
create activityUuid name measurementUuid duration measuredAt =
  ActivityAggregate (activity, pure measurement)
  where
    activity = Activity.create activityUuid name
    measurement = Measurement.create measurementUuid (getId activity) duration measuredAt

measure :: ActivityAggregate -> UUID -> Duration -> UTCTime -> ActivityAggregate
measure (ActivityAggregate (activity, measurements)) measurementUuid duration measuredAt =
  ActivityAggregate (activity, measurement <| measurements)
  where
    measurement = Measurement.create measurementUuid (getId activity) duration measuredAt

predictDuration :: ActivityAggregate -> Duration
predictDuration (ActivityAggregate (_, measurements)) =
  Measurement.predictDuration measurements

-- Instances

instance Show ActivityAggregate where
  show (ActivityAggregate (activity, _)) = show activity
