module ActivityAggregate.RepositoryTests (repositoryTests) where

import ActivityAggregate (ActivityAggregate, ActivityId)
import qualified ActivityAggregate
import ActivityAggregate.Repository (ActivityRepository, RepositoryError)
import qualified ActivityAggregate.Repository as Repository
import ActivityAggregate.Repository.State (ActivityMap)
import qualified ActivityAggregate.Repository.State as Repository
import Control.Category ((>>>))
import qualified Data.Either as Either
import Data.Function ((&))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime)
import Duration (Duration)
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import qualified Polysemy.Error as Error
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import qualified Polysemy.State as State
import System.Random (RandomGen)
import qualified System.Random as Random
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import Test.Tasty.QuickCheck (Property, (===))
import qualified Test.Tasty.QuickCheck as QC
import qualified Utils
import qualified Utils.Entity as Entity
import Utils.NonEmptyText (NonEmptyText)

repositoryTests :: TestTree
repositoryTests =
  Tasty.testGroup
    "Activity Aggregate Repository Module"
    [createTests, measureTests]

createTests :: TestTree
createTests =
  Tasty.testGroup
    "Create Function"
    [ QC.testProperty "should always add one more Activity" createTest
    ]

measureTests :: TestTree
measureTests =
  Tasty.testGroup
    "Measure Function"
    [ QC.testProperty "should fail if there's not an Activity of the given ID" notFoundMeasureTest,
      QC.testProperty "should add a new Measurement to an Activity if it exists" foundMeasureTest
    ]

createTest ::
  NonEmptyText ->
  Duration ->
  UTCTime ->
  Int ->
  ActivityMap ->
  Property
createTest activityName activityDuration time seed activityMap =
  Map.size activityMap + 1 === Map.size newActivityMap
  where
    newActivityMap = Polysemy.run program
    program =
      Repository.create activityName activityDuration
        & consumeStoreRandomAndInputPure time rng activityMap
    rng = Random.mkStdGen seed

notFoundMeasureTest :: ActivityId -> Duration -> UTCTime -> Int -> Bool
notFoundMeasureTest activityId activityDuration time seed =
  Either.isLeft result
  where
    result = Polysemy.run program
    program =
      Repository.measure activityId activityDuration
        & consumeStoreRandomAndInputPure time rng Map.empty
        & Error.runError @RepositoryError
    rng = Random.mkStdGen seed

foundMeasureTest :: ActivityAggregate -> Duration -> UTCTime -> Int -> Bool
foundMeasureTest activity activityDuration time seed =
  either (const False) compareNewActivity result
  where
    oldLength = ActivityAggregate.getMeasurements activity & NonEmpty.length
    getNewLength = ActivityAggregate.getMeasurements >>> NonEmpty.length
    compareNewActivity =
      Map.lookup activityId
        >>> maybe False (getNewLength >>> (==) (oldLength + 1))
    result = Polysemy.run program
    program =
      Repository.measure activityId activityDuration
        & consumeStoreRandomAndInputPure time rng activityMap
        & Error.runError @RepositoryError
    activityMap = Map.singleton activityId activity
    activityId = Entity.getId activity
    rng = Random.mkStdGen seed

-- Helper functions

consumeStoreRandomAndInputPure ::
  RandomGen gen =>
  UTCTime ->
  gen ->
  ActivityMap ->
  Sem (ActivityRepository : Random : Input UTCTime : r) a ->
  Sem r ActivityMap
consumeStoreRandomAndInputPure time rng activityMap =
  Repository.runActivityRepositoryAsState
    >>> State.runState activityMap
    >>> Random.runRandom rng
    >>> fmap snd
    >>> Input.runInputConst time
    >>> fmap fst
