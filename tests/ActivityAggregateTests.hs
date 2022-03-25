module ActivityAggregateTests (activityAggregateTests) where

import qualified ActivityAggregate
import qualified Data.Time.Clock as Time
import qualified Data.UUID.V4 as UUID
import NonEmptyString (NonEmptyString)
import qualified NonEmptyString
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HUnit
import qualified Utils

activityAggregateTests :: TestTree
activityAggregateTests =
  HUnit.testCaseSteps "Activity Aggregate Module" $ \step -> do
    step "Create Function"
    activityUuid <- UUID.nextRandom
    measurementUuid <- UUID.nextRandom
    measuredAt <- Time.getCurrentTime
    let activityAggregate = ActivityAggregate.create activityUuid Utils.running measurementUuid 3600 measuredAt

    step "Measure Function"
    measurementUuid2 <- UUID.nextRandom
    measuredAt2 <- Time.getCurrentTime
    let activityAggregate2 = ActivityAggregate.measure activityAggregate measurementUuid2 10800 measuredAt2

    step "Predict Duration Function"
    ActivityAggregate.predictDuration activityAggregate2 @?= 7200
