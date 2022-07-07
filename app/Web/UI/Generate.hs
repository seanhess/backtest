module Web.UI.Generate where

import Prelude
import qualified Data.Text as Text
import Data.Text (Text)
import Web.UI.Types
import Web.UI.Attributes
import Data.Proxy



stylesheet :: (ToClass color, ToValue color, ToClass space, ToValue space) => [color] -> [space] -> Text
stylesheet colors spaces = Text.intercalate "\n" (generate colors spaces)


-- could allow you to pass in your own space sizes
generate :: (ToClass color, ToValue color, ToClass spaces, ToValue spaces) => [color] -> [spaces] -> [Text]
generate colors spaces = mconcat $
  [ genClasses (range :: [Flex])
  , genClasses $ do
      side <- All : (Side <$> range)
      size <- range
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

classDefinition :: ToClass c => c -> Text
classDefinition c = "." <> className c <> " { " <> (Text.intercalate "; " $ (classStyles c)) <> " }"

-- oh, that's not that great
genClasses :: forall c. (ToClass c) => [c] -> [Text]
genClasses cls = map classDefinition cls