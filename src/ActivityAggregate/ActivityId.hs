module ActivityAggregate.ActivityId (ActivityId, create) where

import Data.UUID (UUID)

newtype ActivityId = ActivityId UUID

create :: UUID -> ActivityId
create = ActivityId
