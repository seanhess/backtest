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
import Web.UI.Attributes
import Web.UI.Generate

import Lucid.Base (makeAttribute)


-- Inspired by Tailwind and Elm UI
-- TODO some way to compose attributes more elegantly?
-- TODO tables


newtype UI t a = UI
  { fromUI :: Html a
  }

instance IsString (UI t ()) where
  fromString s = UI $ (toHtml $ pack s)


-- should also do something?
layout :: UI t () -> Html ()
layout ui = fromUI ui

none :: UI t ()
none = UI $ pure ()

-- TODO Atttributes
-- should it BE html?

el :: [Attribute] -> UI t () -> UI t ()
el as ct = UI $
  div_ as (fromUI ct)

text :: Text -> UI t ()
text = UI . toHtml

row :: [Attribute] -> UI t () -> UI t ()
row as ct = UI $
  div_ (flex Row : as) (fromUI ct)

col :: [Attribute] -> UI t () -> UI t ()
col as ct = UI $
  div_ (flex Col : as) (fromUI ct)

paragraph :: [Attribute] -> UI t a -> UI t a
paragraph as cnt = UI $ p_ as (fromUI cnt)




-- seems kinda silly
-- border :: Show color => color -> Attribute
-- border c = classes [BC c, BW T B1, BW B B1, BW L B1, BW R B1 ]


-- data Event
--   = OnClick Text

-- instance ToAttribute Event where
--   toAttribute (OnClick act) = makeAttribute "data-click" act

onClick :: Text -> Attribute
onClick t = makeAttribute "data-onclick" t

example :: UI t ()
example = do
  -- oh no....
  -- you can't put them together into the same list
  row [ bg Purple, flex Col, onClick "asdf" ] "hello"




data AppColor
  = BlackSoft
  | GrayStone
  | Purple
  | PurpleLight
  | Green
  | GreenHover
  | White
  deriving (Show, Eq, Bounded, Enum)
instance Segment AppColor
instance Value AppColor where
  units BlackSoft = Color "555"
  units GrayStone = Color "999"
  units Purple    = Color "510C76"
  units PurpleLight = Color "862F8B"
  units Green      = Color "93E852"
  units GreenHover = Color "BDF294"
  units White      = Color "FFF"

 