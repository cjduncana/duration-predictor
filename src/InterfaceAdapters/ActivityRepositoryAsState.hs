-- |
-- Module: Activity Aggregate Repository as State
-- Description: Interpret the Activity Repository with Polysemy's State
--
-- This module interprets the Activity Repository using Polysemy's State monad.
-- It does this by storing and manipulating a map of Activities in the State.
module InterfaceAdapters.ActivityRepositoryAsState
  ( ActivityMap,
    runActivityRepositoryAsState,
  )
where

import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Domain.ActivityAggregate (ActivityAggregate, ActivityId)
import Polysemy (Sem)
import qualified Polysemy
import Polysemy.State (State)
import qualified Polysemy.State as State
import UseCases.ActivityRepository
  ( ActivityRepository
      ( Create,
        Get,
        ListActivities,
        Update
      ),
  )
import qualified Utils.Entity as Entity

-- | A map of Activities where the keys are its IDs
type ActivityMap = Map ActivityId ActivityAggregate

-- | Turn the Activity Repository effect into Polysemy's State
runActivityRepositoryAsState ::
  -- | A program with the Activity Repository as its next step
  Sem (ActivityRepository : r) a ->
  -- | A program with Polysemy's State holding the Activity map as its next step
  Sem (State ActivityMap : r) a
runActivityRepositoryAsState = Polysemy.reinterpret $ \case
  ListActivities -> State.gets Map.elems
  Create activity -> insert activity
  Get activityId -> Map.lookup activityId & State.gets
  Update activity -> insert activity
  where
    insert activity =
      Map.insert (Entity.getId activity) activity
        & State.modify
        >> return activity