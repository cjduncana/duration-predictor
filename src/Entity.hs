-- |
-- Module: Entity
-- Description: Data that has an identity (ID) and its fields changes over time
--
-- Entities (also known as Reference Objects) are not fundamentally defined by
-- their fields, but rather by a thread of continuity and identity.
--
-- Each Entity is uniquely identified by an ID rather than by any other field;
-- therefore, two Entities can be considered equal (identifier equality) if both
-- of them have the same ID even though they have different fields. This means
-- that the state of the Entity can be changed anytime, but as long as two
-- Entities have the same ID, both are considered equal regardless what fields
-- they have.
module Entity (Entity, getId, equal) where

-- | Any data that has an identity (ID)
class Entity a id | a -> id where
  -- | Will output the ID for any Entity
  getId :: a -> id

-- | Two Entities are equal if they share the same identity
equal :: (Eq id, Entity a id) => a -> a -> Bool
equal a b = getId a Prelude.== getId b
