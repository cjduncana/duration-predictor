module ActivityAggregate.Repository.Internal
  ( ActivityRepository (Create, Get, ListActivities, Update),
    create,
    get,
    update,
  )
where

import ActivityAggregate (ActivityAggregate)
import ActivityAggregate.ActivityId (ActivityId)
import qualified Polysemy

-- | A repository to create and get Activities
data ActivityRepository m a where
  ListActivities :: ActivityRepository m [ActivityAggregate]
  Create :: ActivityAggregate -> ActivityRepository m ActivityAggregate
  Get :: ActivityId -> ActivityRepository m (Maybe ActivityAggregate)
  Update :: ActivityAggregate -> ActivityRepository m ActivityAggregate

Polysemy.makeSem ''ActivityRepository