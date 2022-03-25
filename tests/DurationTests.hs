module DurationTests (durationTests) where

import qualified Data.Either as Either
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time.Clock (NominalDiffTime)
import Duration (Duration)
import qualified Duration
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import Test.Tasty.QuickCheck (Arbitrary, Gen, Property, (.&&.), (===), (==>))
import qualified Test.Tasty.QuickCheck as QC
import qualified Utils

durationTests :: TestTree
durationTests =
  Tasty.testGroup
    "Duration Module"
    [createTests, meanTests, numTests, fractionalTests]

createTests :: TestTree
createTests =
  Tasty.testGroup
    "Create Function"
    [ QC.testProperty
        "should return a valid Duration if given a non-negative value"
        validDurationTest,
      QC.testProperty
        "should return an invalid Duration if given a negative value"
        invalidDurationTest
    ]

meanTests :: TestTree
meanTests =
  Tasty.testGroup
    "Mean Function"
    [ QC.testProperty
        "should return a zero Duration if all the Durations are zeroes"
        zeroMeanTest
    ]

numTests :: TestTree
numTests =
  Tasty.testGroup
    "Num Duration Instance"
    [ QC.testProperty
        "should follow Associativity of (+)"
        additiveAssociativityTest,
      QC.testProperty
        "should follow Commutativity of (+)"
        commutativityTest,
      QC.testProperty
        "should assign 0 as the additive identity"
        additiveIdentityTest,
      QC.testProperty
        "should assign the additive inverse as the additive identity"
        additiveInverseTest,
      QC.testProperty
        "should follow Associativity of (*)"
        multiplicativeAssociativityTest,
      QC.testProperty
        "should assign 1 as the multiplicative identity"
        multiplicativeIdentityTest,
      QC.testProperty
        "should follow Distributivity of (*) with respect to (+)"
        distributivityTest
    ]

fractionalTests :: TestTree
fractionalTests =
  Tasty.testGroup
    "Fractional Duration Instance"
    [ QC.testProperty
        "should not guarantee a multiplicative inverse for every Duration"
        (QC.expectFailure multiplicativeInverseTest)
    ]

validDurationTest :: Property
validDurationTest =
  QC.forAll (QC.getNonNegative <$> QC.arbitrary) $
    \duration -> Either.isRight (Duration.create duration)

invalidDurationTest :: Property
invalidDurationTest =
  QC.forAll (QC.getNegative <$> QC.arbitrary) $
    \duration -> Either.isLeft (Duration.create duration)

zeroMeanTest :: Property
zeroMeanTest =
  QC.forAll (Utils.constDurations 0) $
    \durations -> Duration.mean durations == 0

additiveAssociativityTest :: Duration -> Duration -> Duration -> Property
additiveAssociativityTest x y z =
  (x + y) + z === x + (y + z)

commutativityTest :: Duration -> Duration -> Property
commutativityTest x y =
  x + y === y + x

additiveIdentityTest :: Duration -> Property
additiveIdentityTest x =
  x + 0 === x

additiveInverseTest :: Duration -> Duration -> Property
additiveInverseTest x y =
  x + negated === x
  where
    negated = negate y

multiplicativeAssociativityTest :: Duration -> Duration -> Duration -> Property
multiplicativeAssociativityTest x y z =
  (x * y) * z === x * (y * z)

multiplicativeIdentityTest :: Duration -> Property
multiplicativeIdentityTest x =
  1 `multiplyBy` x === x .&&. x `multiplyBy` 1 === x
  where
    multiplyBy = (*)

distributivityTest :: Duration -> Duration -> Duration -> Property
distributivityTest a b c =
  a * (b + c) === (a * b) + (a * c) .&&. (b + c) * a === (b * a) + (c * a)

multiplicativeInverseTest :: Duration -> Property
multiplicativeInverseTest x =
  x * recip x === 1 .&&. recip x * x === 1
