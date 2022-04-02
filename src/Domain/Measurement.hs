module Domain.Measurement (Measurement, create, predictDuration) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
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
predictDuration :: NonEmpty Measurement -> Duration
predictDuration measurements =
  measurements <&> duration
    & Duration.mean
