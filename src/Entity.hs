module Entity (Entity, getId) where

class Entity a id where
  getId :: a -> id
