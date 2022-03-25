-- |
-- Module: Non-Empty String
-- Description: A String guaranteed to have at least one character
--
-- This modules guarantees that a 'NonEmptyString' has at least one 'Char'.
module NonEmptyString
  ( NonEmptyString,
    NonEmptyStringError (EmptyString),
    create,
    build,
  )
where

-- | A String guaranteed to have at least one character
newtype NonEmptyString = NonEmptyString String

-- | Possible reasons why the code will not create a 'NonEmptyString'
data NonEmptyStringError
  = -- | This module guarantees that all 'NonEmptyString' have at least one
    -- 'Char'
    EmptyString
  deriving (Show)

-- | Smart constructor for 'NonEmptyString' for any 'String'
create :: String -> Either NonEmptyStringError NonEmptyString
create string =
  if null string
    then Left EmptyString
    else Right (NonEmptyString string)

-- | Smart constructor for 'NonEmptyString' when you have at least one 'Char'
build :: Char -> String -> NonEmptyString
build character string = NonEmptyString (character : string)

-- Instances

instance Show NonEmptyString where
  show (NonEmptyString string) = string
