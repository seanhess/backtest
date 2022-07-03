module Web.UI.Generate where

import Prelude
import qualified Data.Text as Text
import Data.Text (Text)
import Web.UI.Types
import Web.UI.Classes

stylesheet :: (ClassName color, ToValue color) => [color] -> Text
stylesheet colors = Text.intercalate "\n" (generate colors)

  -- wait, it's impossible to generate this without specifying the color! :(
generate :: (ClassName color, ToValue color) => [color] -> [Text]
generate colors = map classDefinition $ mconcat 
  [ fmap flex range
  , fmap background colors
  -- , fmap BC range
  -- , do side <- range
  --      size <- range
  --      pure $ BW side size
  -- , fmap BG range
  ]

range :: (Enum a, Bounded a) => [a]
range = [minBound..maxBound]


classDefinition :: Class -> Text
classDefinition (Class n ss) = "." <> n <> " { " <> (Text.intercalate ";" $ ss) <> " }"