module ActivityAggregate.ActivityId (ActivityId, create) where

import Data.UUID (UUID)

-- | An Activity's identity
newtype ActivityId = ActivityId UUID deriving (Eq, Ord, Show)

create :: UUID -> ActivityId
create = ActivityId
