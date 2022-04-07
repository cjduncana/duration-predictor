module Domain.Activity
  ( Activity,
    create,
    averageMeasurement,
    addMeasurement,
  )
where

import Data.Ratio (Ratio)
import Data.UUID (UUID)
import Domain.ActivityId (ActivityId)
import qualified Domain.ActivityId as ActivityId
import qualified Domain.Duration as Duration
import Domain.Measurement (Measurement)
import qualified Domain.Measurement as Measurement
import Utils.Entity (Entity)
import qualified Utils.Entity as Entity
import Utils.NonEmptyText (NonEmptyText)

newtype Name = Name NonEmptyText

data Activity = Activity
  { id :: ActivityId,
    name :: Name,
    averageMeasurement :: Ratio Int,
    countMeasurements :: Int
  }

create :: UUID -> NonEmptyText -> Ratio Int -> Int -> Activity
create id name averageMeasurement countMeasurements =
  Activity
    { Domain.Activity.id = ActivityId.create id,
      name = Name name,
      averageMeasurement = averageMeasurement,
      countMeasurements = countMeasurements
    }

addMeasurement :: Measurement -> Activity -> Activity
addMeasurement
  measurement
  activty@Activity {averageMeasurement = a, countMeasurements = n} =
    activty {averageMeasurement = nextAverage, countMeasurements = n + 1}
    where
      nextAverage = Duration.nextAverage n a $ Measurement.duration measurement

-- Instances

instance Show Name where
  show (Name name) = show name

instance Show Activity where
  show = show . name

instance Entity Activity ActivityId where
  getId = Domain.Activity.id

instance Eq Activity where
  (==) = Entity.equal
