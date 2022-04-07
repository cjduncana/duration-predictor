-- |
-- Module: Activity Aggregate Use Cases
-- Description: Use Case layer for the Activity system
--
-- This module specifies the Use Case layer for the Reservation system. It
-- coordinates access to Effects and the actual domain logic. The module exposes
-- service functions that will be used by the REST API in the External layer.
module UseCases.ActivityAggregate
  ( ActivityRepository,
    ActivityError (ActivityNotFound),
    create,
    measure,
    predict,
    Repository.listActivities,
  )
where

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.Ratio (Ratio)
import Data.Time.Clock (UTCTime)
import Domain.ActivityAggregate (ActivityAggregate)
import qualified Domain.ActivityAggregate as ActivityAggregate
import Domain.ActivityId (ActivityId)
import Domain.Duration (Duration)
import Polysemy (Members, Sem)
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import UseCases.ActivityRepository (ActivityRepository)
import qualified UseCases.ActivityRepository as Repository
import Utils.NonEmptyText (NonEmptyText)

-- | Possible reasons why this module might fail
newtype ActivityError
  = -- | If this module cannot get a Activity in the repository
    ActivityNotFound ActivityId

-- | Create a new Activity in the repository
create ::
  Members '[ActivityRepository, Random, Input UTCTime] r =>
  NonEmptyText ->
  Duration ->
  Sem r ActivityAggregate
create name = mcreate name >=> Repository.create

-- | Add a new Measurement to an existing Activity
measure ::
  Members '[ActivityRepository, Random, Input UTCTime, Error ActivityError] r =>
  ActivityId ->
  Duration ->
  Sem r ActivityAggregate
measure activityId duration = do
  existingActivity <- getActivity activityId
  mmeasure existingActivity duration >>= Repository.update

-- | Calculate how many seconds the next 'Measurement' will take based on
-- existing Measurements
predict ::
  Members '[ActivityRepository, Error ActivityError] r =>
  ActivityId ->
  Sem r (Ratio Int)
predict activityId =
  getActivity activityId <&> ActivityAggregate.predict

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
  Members '[ActivityRepository, Error ActivityError] r =>
  ActivityId ->
  Sem r ActivityAggregate
getActivity activityId =
  Repository.get activityId >>= Error.note (ActivityNotFound activityId)
