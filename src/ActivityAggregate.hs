-- |
-- Module: Activity Aggregate
-- Description: Functions relating Activities and their Measurements
--
-- This module contain functions relating Activities and their Measurements.
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

-- | An 'Activity' and at least one 'Measurement'
newtype ActivityAggregate = ActivityAggregate (Activity, NonEmpty Measurement)

-- | Create an 'Activity' and its first 'Measurement'
create ::
  -- | New 'Activity' ID
  UUID ->
  -- | New 'Activity' Name
  NonEmptyString ->
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
    measurement = Measurement.create measurementUuid (getId activity) duration measuredAt

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
    measurement = Measurement.create measurementUuid (getId activity) duration measuredAt

-- | Predict the next 'Duration' based on previous 'Measurement'
predictDuration :: ActivityAggregate -> Duration
predictDuration (ActivityAggregate (_, measurements)) =
  Measurement.predictDuration measurements

-- Instances

instance Show ActivityAggregate where
  show (ActivityAggregate (activity, _)) = show activity
