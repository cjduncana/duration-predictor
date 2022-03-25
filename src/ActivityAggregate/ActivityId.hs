module ActivityAggregate.ActivityId (ActivityId, create) where

import Data.UUID (UUID)

newtype ActivityId = ActivityId UUID deriving (Eq)

create :: UUID -> ActivityId
create = ActivityId
