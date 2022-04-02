-- |
-- Module: Activity Aggregate
-- Description: Functions relating Activities and their Measurements
--
-- This module contain functions relating Activities and their Measurements.
module Domain.ActivityAggregate
  ( ActivityAggregate,
    ActivityId,
    create,
    createActivityId,
    getMeasurements,
    measure,
    predictDuration,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.Activity (Activity)
import qualified Domain.Activity as Activity
import Domain.ActivityId (ActivityId)
import qualified Domain.ActivityId as ActivityId
import Domain.Duration (Duration)
import qualified Domain.Duration as Duration
import Domain.Measurement (Measurement)
import qualified Domain.Measurement as Measurement
import Utils.Entity (Entity)
import qualified Utils.Entity as Entity
import Utils.NonEmptyText (NonEmptyText)

-- | An 'Activity' and at least one 'Measurement'
newtype ActivityAggregate = ActivityAggregate (Activity, NonEmpty Measurement)

-- | Create an 'Activity' and its first 'Measurement'
create ::
  -- | New 'Activity' ID
  UUID ->
  -- | New 'Activity' Name
  NonEmptyText ->
  -- | New 'Measurement' ID
  UUID ->
  -- | Amount of time elapsed completing this 'Activity'
  Duration ->
  -- | When did the 'Measurement' took place
  UTCTime ->
  ActivityAggregate
create activityUuid name measurementUuid duration measuredAt =
  ActivityAggregate (activity, pure measurement)
  where
    activity = Activity.create activityUuid name
    measurement =
      Measurement.create
        measurementUuid
        (Entity.getId activity)
        duration
        measuredAt

-- | Add a new 'Measurement' to this 'Activity'
measure ::
  -- | 'Activity' measured
  ActivityAggregate ->
  -- | New 'Measurement' ID
  UUID ->
  -- | Amount of time elapsed completing this 'Activity'
  Duration ->
  -- | When did the 'Measurement' took place
  UTCTime ->
  ActivityAggregate
measure (ActivityAggregate (activity, measurements)) measurementUuid duration measuredAt =
  ActivityAggregate (activity, measurement <| measurements)
  where
    measurement =
      Measurement.create
        measurementUuid
        (Entity.getId activity)
        duration
        measuredAt

-- | Predict the next 'Duration' based on previous 'Measurement'
predictDuration :: ActivityAggregate -> Duration
predictDuration (ActivityAggregate (_, measurements)) =
  Measurement.predictDuration measurements

-- | Create an Activity's identity without the Activity
createActivityId :: UUID -> ActivityId
createActivityId = ActivityId.create

-- | Get the Measurements of an Activity
getMeasurements :: ActivityAggregate -> NonEmpty Measurement
getMeasurements (ActivityAggregate (_, measurements)) = measurements

-- Instances

instance Show ActivityAggregate where
  show (ActivityAggregate (activity, _)) = show activity

instance Entity ActivityAggregate ActivityId where
  getId (ActivityAggregate (activity, _)) = Entity.getId activity

instance Eq ActivityAggregate where
  (==) = Entity.equal