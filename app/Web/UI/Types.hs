{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.UI.Types where

import Prelude
import Data.Text as Text (Text, unpack, pack, toLower, replace, splitOn)
import Lucid (Attribute, class_)
import Numeric (showFFloat)


class ToValue a where
  value :: a -> Units


class ToStyle a where
  styleName :: a -> Text

data Units
  = Px Int
  | Color Text
  | Rem Float
instance Show Units where
  show (Px n) = show n <> "px"
  show (Color t) = "#" <> unpack t
  show (Rem f) = showFFloat (Just 3) f "rem"




(-) :: Text -> Text -> Text
a - ""  = a
a - b  = a <> "-" <> b

(.:) :: ToValue v => Text -> v -> Text
a .: b = a <> ":" <> (pack $ show $ value b)


-- serializes to a class. Other attributes can be different!
-- instance ToAttribute Class where
--   toAttribute (Class n _) = class_ (n <> " ")


-- it's kind of weird to have both
class ToClass a where
  className :: a -> Text
  classStyles :: a -> [Text]

  default className :: Show a => a -> Text
  className c = Text.replace " " "-" $ Text.toLower $ pack $ show c

  default classStyles :: a -> [Text]
  classStyles _ = []

