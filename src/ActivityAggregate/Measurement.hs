module ActivityAggregate.Measurement (Measurement, create, predictDuration) where

import ActivityAggregate.ActivityId (ActivityId)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Duration (Duration)
import qualified Duration

newtype MeasurementId = MeasurementId UUID

data Measurement = Measurement
  { id :: MeasurementId,
    activityId :: ActivityId,
    duration :: Duration,
    measuredAt :: UTCTime
  }

create :: UUID -> ActivityId -> Duration -> UTCTime -> Measurement
create id activityId duration measuredAt =
  Measurement
    { ActivityAggregate.Measurement.id = MeasurementId id,
      activityId = activityId,
      duration = duration,
      measuredAt = measuredAt
    }

-- First naive prediction: averaging
predictDuration :: NonEmpty Measurement -> Duration
predictDuration measurements =
  measurements <&> duration
    & Duration.mean
