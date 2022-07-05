module Web.UI
  ( stylesheet, AppColor(..), range

  ) where

import Prelude hiding ((-))
import Lucid
import Lucid.Html5
import Data.Text (Text, pack, unpack)
import Data.ByteString (ByteString)
import Text.RawString.QQ (r)
import Data.String (IsString(..))
import qualified Data.Text as Text
import Web.UI.Types
import Web.UI.Classes
import Web.UI.Generate

import Lucid.Base (makeAttribute)


-- Inspired by Tailwind and Elm UI



newtype UI t a = UI { fromUI :: Html a }

instance IsString (UI t ()) where
  fromString s = UI $ (toHtml $ pack s)


-- should also do something?
layout :: UI t () -> Html ()
layout ui = fromUI ui

none :: UI t ()
none = UI $ pure ()

-- TODO Atttributes
el :: ToAttribute att => [att] -> UI t () -> UI t ()
el as ct = UI $ do
  div_ (map toAttribute as) (fromUI ct)


-- seems kinda silly
-- border :: Show color => color -> Attribute
-- border c = classes [BC c, BW T B1, BW B B1, BW L B1, BW R B1 ]


data Event
  = OnClick Text

instance ToAttribute Event where
  toAttribute (OnClick act) = makeAttribute "data-click" act

example :: UI t ()
example = do
  -- oh no....
  -- you can't put them together into the same list
  row [ bg Purple ] "hello"


row :: (ToAttribute att) => [att] -> UI t a -> UI t a
row as cnt = UI $ div_ (toAttribute (Flex Row) : map toAttribute as) (fromUI cnt)

-- hmm... these helpers need to go directly to attributes, no?
-- or just duplicate them all. Classes
-- the nicely named functions need to be for users to use in their attributes

-- but... but.... I want a bunch of classes
-- background' is unsatisfying

-- col :: [Attribute] -> UI content a -> UI layout a
-- col as cnt = UI $ div_ (class_ (className (Flex Col)) : as) (fromUI cnt)

-- The point of this was that I didn't want to have to specify them twice, right?
-- can I do without Class completely?

-- toStyles :: (ToValue color) => Class color -> [Text]
-- toStyles (Flex Col)     = ["display:flex", "flex-direction:column"]
-- toStyles (Flex Row)     = ["display:flex", "flex-direction:row"]
-- toStyles (BW side size) = ["border"-(styleName side)-"width" .: size]
-- toStyles (BC color)     = ["border-color" .: color]
-- toStyles (BG color)     = ["background-color" .: color]



data AppColor
  = BlackSoft
  | GrayStone
  | Purple
  | PurpleLight
  | Green
  | GreenHover
  | White
  deriving (Show, Eq, Bounded, Enum)
instance ClassName AppColor where
  className = pack . show

instance ToValue AppColor where
  value BlackSoft = Color "555"
  value GrayStone = Color "999"
  value Purple    = Color "510C76"
  value PurpleLight = Color "862F8B"
  value Green      = Color "93E852"
  value GreenHover = Color "BDF294"
  value White      = Color "FFF"

 