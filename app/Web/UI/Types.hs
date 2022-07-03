module Web.UI.Types where

import Prelude
import Data.Text (Text, unpack, pack)
import Lucid (Attribute, class_)


class ToValue a where
  value :: a -> Units

class ToStyle a where
  styleName :: a -> Text

data Units
  = Px Int
  | Color Text
  deriving (Eq)
instance Show Units where
  show (Px n) = show n <> "px"
  show (Color t) = "#" <> unpack t

(-) :: Text -> Text -> Text
a - b = a <> "-" <> b

(.:) :: ToValue val => Text -> val -> Text
a .: b = a <> ":" <> (pack $ show $ value b)


data Class = Class
  { name   :: Text
  , styles :: [Text]
  }

-- serializes to a class. Other attributes can be different!
instance ToAttribute Class where
  toAttribute (Class n _) = class_ (n <> " ")

class ClassName a where
  className :: a -> Text

class ToAttribute a where
  toAttribute :: a -> Attribute
