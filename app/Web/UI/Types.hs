{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.UI.Types where

import Prelude
import Data.Text as Text (Text, unpack, pack, toLower, replace, splitOn)
import Lucid (Attribute, class_)
import Numeric (showFFloat)


class Segment a where
  segment :: a -> Text

  default segment :: Show a => a -> Text
  segment c = Text.replace " " "" $ Text.toLower $ pack $ show c

class Segment a => Value a where
  units :: a -> Units

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

(.:) :: Value a => Text -> a -> Text
a .: b =
  a <> ":" <> (pack $ show $ units b)

-- asdf :: Value typ a => a -> Text
-- asdf = segment


data Class_ = Class
  { className :: Text
  , styles :: [Text]
  }

class Class a where
  toClass :: a -> Class_

