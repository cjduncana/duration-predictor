module Utils where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time.Clock (NominalDiffTime)
import Duration (Duration)
import qualified Duration
import NonEmptyString (NonEmptyString)
import qualified NonEmptyString
import Test.Tasty.QuickCheck (Arbitrary, Gen)
import qualified Test.Tasty.QuickCheck as QC

-- Helper functions

constDurations :: a -> Gen (NonEmpty a)
constDurations duration =
  (:|)
    <$> constDuration
    <*> QC.listOf constDuration
  where
    constDuration = pure duration

running :: NonEmptyString
running = NonEmptyString.build 'R' "unning"

-- Instances

instance Arbitrary NominalDiffTime where
  arbitrary = fromInteger <$> QC.arbitrary

instance Arbitrary Duration where
  arbitrary =
    QC.suchThatMap
      (Duration.create <$> QC.arbitrary)
      (either (const Nothing) Just)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> QC.arbitrary <*> QC.arbitrary