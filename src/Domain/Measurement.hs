module Domain.Measurement (Measurement, create, duration, mean) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Ratio (Ratio)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.ActivityId (ActivityId)
import Domain.Duration (Duration)
import qualified Domain.Duration as Duration

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
    { Domain.Measurement.id = MeasurementId id,
      activityId = activityId,
      duration = duration,
      measuredAt = measuredAt
    }

-- First naive prediction: averaging
mean :: NonEmpty Measurement -> Ratio Int
mean measurements =
  measurements <&> duration
    & Duration.mean
