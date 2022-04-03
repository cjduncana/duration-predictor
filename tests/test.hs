import Domain.ActivityAggregateTests (activityAggregateTests)
import Domain.DurationTests (durationTests)
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import qualified UseCases.ActivityAggregateTests
import Utils.NonEmptyTextTests (nonEmptyTextTests)

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests =
  Tasty.testGroup
    "Duration Predictor Tests"
    [ activityAggregateTests,
      UseCases.ActivityAggregateTests.useCasesTests,
      durationTests,
      nonEmptyTextTests
    ]