module Domain.Activity (Activity, create) where

import Data.UUID (UUID)
import Domain.ActivityId (ActivityId)
import qualified Domain.ActivityId as ActivityId
import Utils.Entity (Entity)
import qualified Utils.Entity as Entity
import Utils.NonEmptyText (NonEmptyText)

newtype Name = Name NonEmptyText

data Activity = Activity
  { id :: ActivityId,
    name :: Name
  }

create :: UUID -> NonEmptyText -> Activity
create id name =
  Activity
    { Domain.Activity.id = ActivityId.create id,
      name = Name name
    }

-- Instances

instance Show Name where
  show (Name name) = show name

instance Show Activity where
  show = show . name

instance Entity Activity ActivityId where
  getId = Domain.Activity.id

instance Eq Activity where
  (==) = Entity.equal
