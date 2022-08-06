module Tailwind.UI
  ( module Tailwind.UI.Types
  , module Tailwind.UI.Attribute
  , module Tailwind.Options
  , module Tailwind.Values
  , module Tailwind.Types
  , module Tailwind.Prefix
  , html, str, div, el, el'
  ) where

import Prelude hiding (div)
import Tailwind.Options
import Tailwind.Values
import Tailwind.Prefix
import Tailwind.Types
import Tailwind.UI.Attribute
import Tailwind.UI.Types
import Lucid (div_, Html, toHtml)
import Data.Text (Text)
import qualified Tailwind.Classes as Tailwind


html :: Html a -> UI t a
html h_ = UI h_

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


