-- |
-- Module: Non-Empty Text
-- Description: A Text guaranteed to have at least one character
--
-- This modules guarantees that a 'NonEmptyText' has at least one 'Char'.
module NonEmptyText
  ( NonEmptyText,
    NonEmptyTextError (EmptyText),
    create,
    toText,
  )
where

import Control.Category ((>>>))
import Data.Text (Text)
import qualified Data.Text as T

-- | A Text guaranteed to have at least one character
newtype NonEmptyText = NonEmptyText Text

-- | Possible reasons why the code will not create a 'NonEmptyText'
data NonEmptyTextError
  = -- | This module guarantees that all 'NonEmptyText' have at least one
    -- non-whitespace character
    EmptyText
  deriving (Show)

-- | Smart constructor for 'NonEmptyText' for any 'Text'
create :: Text -> Either NonEmptyTextError NonEmptyText
create text =
  if T.length stripped <= 0
    then Left EmptyText
    else Right (NonEmptyText stripped)
  where
    stripped = T.strip text

toText :: NonEmptyText -> Text
toText (NonEmptyText text) = text

-- Instances

instance Show NonEmptyText where
  show (NonEmptyText text) = T.unpack text
