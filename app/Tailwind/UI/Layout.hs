module Tailwind.UI.Layout where

import Prelude
import Tailwind (Direction(..))
import Lucid (div_)
import Tailwind.UI.Types
import Tailwind.UI.Classes

row :: (Opt p -> Opt p) -> UI a -> UI a
row f ct = UI $ div_ (atts $ f . flex Row . flex () $ opt) (fromUI ct)

col :: (Opt p -> Opt p) -> UI a -> UI a
col f ct = UI $ div_ (atts $ f . flex Col . flex () $ opt) (fromUI ct)

el :: (Opt c -> Opt c) -> UI a -> UI a
el f ct = UI $ div_ (atts $ f opt) (fromUI ct)
