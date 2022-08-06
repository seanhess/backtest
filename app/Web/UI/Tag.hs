module Web.UI.Tag where

import Prelude hiding ((-))
import Tailwind.Types
import Web.UI.Types (Opt, Att, atts, opt, addAttribute)
import Web.UI.Attribute (flex, grow)
import Lucid (Html, Attribute)
import Lucid.Html5 (button_, div_)
import Data.Text (Text)
import qualified Tailwind.Classes as Tailwind
import qualified Tailwind.Prefix as Prefix
import Tailwind.Values (Direction(Row, Col))
import Juniper (PageAction, onClick)

-- could have an img, button, etc
-- unless: maybe, we always use a container instead?
el :: Att a -> Html a -> Html a
el f ct = div_ (atts $ f opt) ct

-- | Simple Layout Tools, you can do most LAYOUT with these. Once you get very dynamic
-- you'll need to use grid or flexbox
row :: (Opt a -> Opt a) -> Html a -> Html a
row f = el (flex Row . flex () . f)

col :: (Opt a -> Opt a) -> Html a -> Html a
col f = el (flex Col . flex () . f)

space :: Html ()
space = el (grow) (pure ())

tag :: ([Attribute] -> Html a -> Html a) -> Att a -> Html a -> Html a
tag tg f ct = tg (atts $ f opt) ct

button :: PageAction action => action -> Att a -> Html a -> Html a
button act f ct = tag button_ (f . addAttribute (onClick act)) ct

