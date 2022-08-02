module Tailwind.UI
  ( module Tailwind.UI.Types
  , module Tailwind.UI.Classes
  , module Tailwind.UI.Layout
  , module Tailwind.Options
  , module Tailwind
  , html, div, el, el', str
  ) where

import Prelude hiding (div)
import Tailwind (Background, Flex, Border, Gap, Padding, Self, Items, Content, Dimensions)
import Tailwind.Options
import Tailwind.UI.Layout
import Tailwind.UI.Types
import Tailwind.UI.Classes
import Lucid (div_, Html, toHtml)
import Data.Text (Text)


html :: Html a -> UI t a
html h = UI h

str :: Text -> UI t ()
str t = UI $ toHtml t

div :: (Opt c -> Opt c) -> UI t a -> UI t a
div f ct = UI $ div_ (atts $ f opt) (fromUI ct)

-- could have an img, button, etc
-- unless: maybe, we always use a container instead?
el :: (Opt c -> Opt c) -> UI t a -> UI t a
el = div

el' :: (Opt c -> Opt c) -> Text -> UI t ()
el' f t = el f (html $ toHtml t)


