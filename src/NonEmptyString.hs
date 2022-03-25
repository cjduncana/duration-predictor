module NonEmptyString
  ( NonEmptyString,
    NonEmptyStringError (EmptyString),
    create,
    build,
  )
where

newtype NonEmptyString = NonEmptyString String

data NonEmptyStringError = EmptyString deriving (Show)

create :: String -> Either NonEmptyStringError NonEmptyString
create string =
  if null string
    then Left EmptyString
    else Right (NonEmptyString string)

build :: Char -> String -> NonEmptyString
build character string = NonEmptyString (character : string)

-- Instances

instance Show NonEmptyString where
  show (NonEmptyString string) = string
