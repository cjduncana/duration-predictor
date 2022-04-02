module Utils.NonEmptyTextTests (nonEmptyTextTests) where

import Control.Category ((>>>))
import qualified Data.Either as Either
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Exts as GHC
import Polysemy.Law (Gen)
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import Test.Tasty.QuickCheck (Property, (===), (==>))
import qualified Test.Tasty.QuickCheck as QC
import qualified Utils.NonEmptyText as NonEmptyText

nonEmptyTextTests :: TestTree
nonEmptyTextTests =
  Tasty.testGroup
    "Non-Empty Text Module"
    [ Tasty.testGroup
        "Create Function"
        [ QC.testProperty
            "should return a valid Non-Empty Text if given a Text of at least one character"
            validNonEmptyTextTest,
          QC.testProperty
            "should return an invalid Non-Empty Text if given a Text with no characters"
            invalidNonEmptyTextTest,
          QC.testProperty
            "should return an invalid Non-Empty Text if given Text of only whitespace"
            invalidBlankTextTest,
          QC.testProperty
            "should return a trimmed Non-Empty Text if the given Text has whitespace in the beginning or in the end"
            trimTextTest
        ]
    ]

validNonEmptyTextTest :: Property
validNonEmptyTextTest =
  QC.forAll nonEmptyText (NonEmptyText.create >>> Either.isRight)

invalidNonEmptyTextTest :: Bool
invalidNonEmptyTextTest = Either.isLeft (NonEmptyText.create $ T.pack "")

invalidBlankTextTest :: Property
invalidBlankTextTest =
  QC.forAll blankText (NonEmptyText.create >>> Either.isLeft)

trimTextTest :: Property
trimTextTest =
  QC.forAll stringTriple $
    \(blank1, text, blank2) ->
      T.length (T.strip text) > 0
        ==> NonEmptyText.create (blank1 `T.append` text `T.append` blank2)
        & either (const (QC.property False)) (NonEmptyText.toText >>> (===) (T.strip text))
  where
    stringTriple =
      (,,)
        <$> blankText
        <*> nonEmptyText
        <*> blankText

nonEmptyText :: Gen Text
nonEmptyText = QC.arbitrary <&> QC.getNonEmpty <&> GHC.fromList

blankText :: Gen Text
blankText = QC.listOf (pure ' ') <&> GHC.fromList
