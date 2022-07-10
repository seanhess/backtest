module Tailwind.Lucid where

import Prelude
import Lucid
import Lucid.Html5 (class_)
import Tailwind.Options
import Data.Text as Text (intercalate)

-- | Convert classes into an Attribute
classes :: [Class] -> Attribute
classes cns = class_ (Text.intercalate " " (map fromClass cns) <> " ")