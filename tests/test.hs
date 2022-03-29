import qualified ActivityAggregate.RepositoryTests
import ActivityAggregateTests (activityAggregateTests)
import DurationTests (durationTests)
import NonEmptyTextTests (nonEmptyTextTests)
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty

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