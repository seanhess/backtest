module Web.UI.Generate where

import Prelude
import qualified Data.Text as Text
import Data.Text (Text)
import Web.UI.Types
import Web.UI.Attributes



stylesheet :: (ClassName color, ToValue color) => [color] -> Text
stylesheet colors = Text.intercalate "\n" (generate colors)


generate :: (ClassName color, ToValue color) => [color] -> [Text]
generate colors = mconcat $
  [ map (classDefinition . toClass) $ mconcat 
    [ fmap (Flex) range
    , do side <- range
         size <- range
         pure $ Pad side size
    ]
  , map (classDefinition . toClass) $ mconcat 
    [ fmap BC colors
    , do side <- range
         size <- range
         pure $ BW side size
    , fmap BG colors
    ]
  ]




range :: (Enum a, Bounded a) => [a]
range = [minBound..maxBound]


classDefinition :: Class -> Text
classDefinition (Class n ss) = "." <> n <> " { " <> (Text.intercalate ";" $ ss) <> " }"