module Web.UI.Generate where

import Prelude
import qualified Data.Text as Text
import Data.Text (Text)
import Web.UI.Types
import Web.UI.Attributes
import Data.Proxy



stylesheet :: (Value color, Value space) => [color] -> [space] -> Text
stylesheet colors spaces = Text.intercalate "\n" (generate colors spaces)


-- you have to pass in your own colors and space sizes, and anything else customizable
-- obviously we can have a default one
-- but we will always need colors? No, we could have a nice default color palette
generate :: (Value color, Value spaces) => [color] -> [spaces] -> [Text]
generate colors spaces = mconcat $
  [ genClasses (range :: [Flex])
  , genClasses $ do
      side <- All : (Side <$> range)
      size <- spaces
      pure $ Pad side size
  , genClasses $ fmap BC colors
  , genClasses $ do
      side <- All : (Side <$> range)
      size <- range
      pure $ BW side size
  , genClasses $ fmap BG colors
  , genClasses $ fmap Gap spaces
  ]




range :: (Enum a, Bounded a) => [a]
range = [minBound..maxBound]

classDefinition :: Class_ -> Text
classDefinition c =
  "." <> className c <> " { " <> (Text.intercalate "; " $ (styles c)) <> " }"

-- oh, that's not that great
genClasses :: forall c. (Class c) => [c] -> [Text]
genClasses cls = map (classDefinition . toClass) cls