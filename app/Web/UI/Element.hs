module Web.UI.Element where

import Prelude
import Tailwind
import Web.UI.Types
import Data.Text (Text)
import Lucid (toHtml, div_)
import Lucid.Base (Term, With(..))

row :: (Term a result, With result) => a -> result
row a = with (div_ a) [ classes [ flex (), flex Row ] ]

col :: (Term a result, With result) => a -> result
col a = with (div_ a) [ classes [ flex (), flex Col ] ]

el :: (Term a result, With result) => a -> result
el a = div_ a

space :: UI ()
space = el [ grow ] (pure () :: UI ())

text :: Text -> UI ()
text t = UI $ toHtml t
