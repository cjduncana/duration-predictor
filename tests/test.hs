import qualified ActivityAggregate.RepositoryTests
import ActivityAggregateTests (activityAggregateTests)
import DurationTests (durationTests)
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import Utils.NonEmptyTextTests (nonEmptyTextTests)

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests =
  Tasty.testGroup
    "Duration Predictor Tests"
    [ activityAggregateTests,
      ActivityAggregate.RepositoryTests.repositoryTests,
      durationTests,
      nonEmptyTextTests
    ]