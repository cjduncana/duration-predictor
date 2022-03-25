module NonEmptyStringTests (nonEmptyStringTests) where

import qualified Data.Either as Either
import qualified NonEmptyString
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import Test.Tasty.QuickCheck (Property, (==>))
import qualified Test.Tasty.QuickCheck as QC

nonEmptyStringTests :: TestTree
nonEmptyStringTests =
  Tasty.testGroup
    "Non-Empty String Module"
    [ Tasty.testGroup
        "Create Function"
        [ QC.testProperty
            "should return a valid Non-Empty String if given a String of at least one character"
            validNonEmptyStringTest,
          QC.testProperty
            "should return an invalid Non-Empty String if given a String no characters"
            invalidNonEmptyStringTest
        ]
    ]

validNonEmptyStringTest :: Property
validNonEmptyStringTest =
  QC.forAll (QC.getNonEmpty <$> QC.arbitrary) $
    \string -> Either.isRight (NonEmptyString.create string)

invalidNonEmptyStringTest :: Bool
invalidNonEmptyStringTest =
  Either.isLeft (NonEmptyString.create "")
