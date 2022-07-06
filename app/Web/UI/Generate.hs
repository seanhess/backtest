module Web.UI.Generate where

import Prelude
import qualified Data.Text as Text
import Data.Text (Text)
import Web.UI.Types
import Web.UI.Attributes

stylesheet :: (ClassName color, ToValue color) => [color] -> Text
stylesheet colors = Text.intercalate "\n" $
  generate0 <> (generate1 colors)


generate0 :: [Text]
generate0 = map (classDefinition . toClass) $ mconcat 
  [ fmap (Flex) range
  , do side <- range
       size <- range
       pure $ Pad side size
  ]


  -- wait, it's impossible to generate this without specifying the color! :(
generate1 :: (ClassName color, ToValue color) => [color] -> [Text]
generate1 colors = map (classDefinition . toClass) $ mconcat 
  [ fmap BC colors
  , do side <- range
       size <- range
       pure $ BW side size
  , fmap BG colors
  ]

range :: (Enum a, Bounded a) => [a]
range = [minBound..maxBound]


classDefinition :: Class -> Text
classDefinition (Class n ss) = "." <> n <> " { " <> (Text.intercalate ";" $ ss) <> " }"