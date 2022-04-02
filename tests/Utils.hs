module Utils (constDurations, running) where

import Control.Monad (Monad)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, NominalDiffTime, UTCTime (UTCTime))
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Domain.ActivityAggregate (ActivityAggregate, ActivityId)
import qualified Domain.ActivityAggregate as ActivityAggregate
import Domain.Duration (Duration)
import qualified Domain.Duration as Duration
import Test.Tasty.QuickCheck (Arbitrary, Gen)
import qualified Test.Tasty.QuickCheck as QC
import Utils.NonEmptyText (NonEmptyText)
import qualified Utils.NonEmptyText as NonEmptyText

constDurations :: a -> Gen (NonEmpty a)
constDurations duration =
  (:|)
    <$> constDuration
    <*> QC.listOf constDuration
  where
    constDuration = pure duration

-- Helper functions

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe = either (const Nothing) Just

running :: NonEmptyText
running =
  NonEmptyText.create (T.pack "Running")
    & either undefined id

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

instance Arbitrary NonEmptyText where
  arbitrary =
    QC.suchThatMap
      (NonEmptyText.create <$> QC.arbitrary)
      eitherToMaybe

instance Arbitrary Duration where
  arbitrary =
    QC.suchThatMap
      (Duration.create <$> QC.arbitrary)
      eitherToMaybe

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> QC.arbitrary <*> QC.arbitrary

instance Arbitrary Text where
  arbitrary = T.pack <$> QC.arbitrary

instance Arbitrary UUID where
  arbitrary = UUID.fromWords64 <$> QC.arbitrary <*> QC.arbitrary

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> QC.arbitrary <*> QC.arbitrary
