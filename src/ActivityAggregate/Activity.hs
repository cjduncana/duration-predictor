module ActivityAggregate.Activity (Activity, create) where

import ActivityAggregate.ActivityId (ActivityId)
import qualified ActivityAggregate.ActivityId as ActivityId
import Data.UUID (UUID)
import Entity (Entity)
import qualified Entity
import NonEmptyText (NonEmptyText)

newtype Name = Name NonEmptyText

data Activity = Activity
  { id :: ActivityId,
    name :: Name
  }

create :: UUID -> NonEmptyText -> Activity
create id name =
  Activity
    { ActivityAggregate.Activity.id = ActivityId.create id,
      name = Name name
    }

-- Instances

instance Show Name where
  show (Name name) = show name

instance Show Activity where
  show = show . name

instance Entity Activity ActivityId where
  getId = ActivityAggregate.Activity.id

instance Eq Activity where
  (==) = Entity.equal
