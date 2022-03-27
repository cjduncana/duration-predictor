module Utils where

import ActivityAggregate (ActivityAggregate, ActivityId)
import qualified ActivityAggregate
import Control.Monad (Monad)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, NominalDiffTime, UTCTime (UTCTime))
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Duration (Duration)
import qualified Duration
import Entity (Entity (getId))
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

instance Arbitrary ActivityAggregate where
  arbitrary =
    ActivityAggregate.create
      <$> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary

instance Arbitrary ActivityId where
  arbitrary = ActivityAggregate.createActivityId <$> QC.arbitrary

instance Arbitrary Day where
  arbitrary = toEnum <$> QC.arbitrary

instance Arbitrary DiffTime where
  arbitrary = fromInteger <$> QC.arbitrary

instance Arbitrary NominalDiffTime where
  arbitrary = fromInteger <$> QC.arbitrary

instance Arbitrary NonEmptyString where
  arbitrary = NonEmptyString.build <$> QC.arbitrary <*> QC.arbitrary

instance Arbitrary Duration where
  arbitrary =
    QC.suchThatMap
      (Duration.create <$> QC.arbitrary)
      (either (const Nothing) Just)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> QC.arbitrary <*> QC.arbitrary

instance Arbitrary UUID where
  arbitrary = UUID.fromWords64 <$> QC.arbitrary <*> QC.arbitrary

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> QC.arbitrary <*> QC.arbitrary
