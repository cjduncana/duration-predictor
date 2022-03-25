import ActivityAggregateTests (activityAggregateTests)
import DurationTests (durationTests)
import NonEmptyStringTests (nonEmptyStringTests)
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests =
  Tasty.testGroup
    "Duration Predictor Tests"
    [activityAggregateTests, durationTests, nonEmptyStringTests]