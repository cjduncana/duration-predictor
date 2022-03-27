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
import qualified Entity
import NonEmptyString (NonEmptyString)
import Polysemy (Member, Members, Sem)
import Polysemy.Error (Error)
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Polysemy.KVStore (KVStore)
import qualified Polysemy.KVStore as KVStore
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random

-- | A repository to create and get Activities
type ActivityRepository = KVStore ActivityId ActivityAggregate

-- | Possible reasons why this module might fail
newtype RepositoryError
  = ActivityNotFound  -- ^ If this module cannot get a Activity in the
                      -- repository
  ActivityId          -- ^ ID of a non-existent Activity

-- | Create a new Activity in the repository
create ::
  Members '[ActivityRepository, Random, Input UTCTime] r =>
  NonEmptyString ->
  Duration ->
  Sem r ()
create name duration = do
  activity <- mcreate name duration
  storeActivity activity

-- | Add a new Measurement to an existing Activity
measure ::
  Members '[ActivityRepository, Random, Input UTCTime, Error RepositoryError] r =>
  ActivityId ->
  Duration ->
  Sem r ()
measure activityId duration = do
  existingActivity <- getActivity activityId
  newActivity <- mmeasure existingActivity duration
  storeActivity newActivity

-- | Calculate the next prediction based on existing Measurements
predictDuration ::
  Members '[ActivityRepository, Error RepositoryError] r =>
  ActivityId ->
  Sem r Duration
predictDuration activityId = do
  existingActivity <- getActivity activityId
  return $ ActivityAggregate.predictDuration existingActivity

-- Helper functions

mcreate ::
  Members '[Random, Input UTCTime] r =>
  NonEmptyString ->
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
  Members '[ActivityRepository, Error RepositoryError] r => ActivityId -> Sem r ActivityAggregate
getActivity = KVStore.lookupOrThrowKV ActivityNotFound

storeActivity :: Member ActivityRepository r => ActivityAggregate -> Sem r ()
storeActivity activity =
  KVStore.writeKV (Entity.getId activity) activity