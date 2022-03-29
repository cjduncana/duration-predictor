-- |
-- Module: Activity Aggregate Repository
-- Description: Interact with the Activity Repository
--
-- This module provides an interface of all Activity behaviors.
module ActivityAggregate.Repository
  ( ActivityRepository,
    RepositoryError (ActivityNotFound),
    create,
    measure,
    predictDuration,
  )
where

import ActivityAggregate (ActivityAggregate)
import qualified ActivityAggregate
import ActivityAggregate.ActivityId (ActivityId)
import Data.Functor ((<&>))
import Data.Time.Clock (UTCTime)
import Duration (Duration)
import NonEmptyText (NonEmptyText)
import Polysemy (Members, Sem)
import Polysemy.Error (Error)
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import Control.Monad ((>=>))
import qualified Polysemy.Error as Error
import ActivityAggregate.Repository.Internal (ActivityRepository)
import qualified ActivityAggregate.Repository.Internal as Internal

-- | Possible reasons why this module might fail
newtype RepositoryError
  = ActivityNotFound  -- ^ If this module cannot get a Activity in the
                      -- repository
  ActivityId          -- ^ ID of a non-existent Activity

-- | Create a new Activity in the repository
create ::
  Members '[ActivityRepository, Random, Input UTCTime] r =>
  NonEmptyText ->
  Duration ->
  Sem r ActivityAggregate
create name = mcreate name >=> Internal.create

-- | Add a new Measurement to an existing Activity
measure ::
  Members '[ActivityRepository, Random, Input UTCTime, Error RepositoryError] r =>
  ActivityId ->
  Duration ->
  Sem r ActivityAggregate
measure activityId duration = do
  existingActivity <- getActivity activityId
  mmeasure existingActivity duration >>= Internal.update

-- | Calculate the next prediction based on existing Measurements
predictDuration ::
  Members '[ActivityRepository, Error RepositoryError] r =>
  ActivityId ->
  Sem r Duration
predictDuration activityId =
  getActivity activityId <&> ActivityAggregate.predictDuration

-- Helper functions

mcreate ::
  Members '[Random, Input UTCTime] r =>
  NonEmptyText ->
  Duration ->
  Sem r ActivityAggregate
mcreate name duration =
  ActivityAggregate.create
    <$> Random.random
    <*> pure name
    <*> Random.random
    <*> pure duration
    <*> Input.input

mmeasure ::
  Members '[Random, Input UTCTime] r =>
  ActivityAggregate ->
  Duration ->
  Sem r ActivityAggregate
mmeasure activity duration =
  ActivityAggregate.measure activity
    <$> Random.random
    <*> pure duration
    <*> Input.input

getActivity ::
  Members '[ActivityRepository, Error RepositoryError] r =>
  ActivityId -> Sem r ActivityAggregate
getActivity activityId =
  Internal.get activityId >>= Error.note (ActivityNotFound activityId)
