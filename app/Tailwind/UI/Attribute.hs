module Tailwind.UI.Attribute where

import Prelude
import qualified Tailwind
import Lucid (div_)
import Tailwind.UI.Types
import Tailwind.Classes
import Tailwind.Values
import Tailwind.Options
import Tailwind.Types



row :: (Opt a -> Opt a) -> UI t () -> UI t ()
row f ct = UI $ div_ (atts $ f $ Opt [] [Tailwind.flex Row, Tailwind.flex ()]) (fromUI ct)

col :: (Opt a -> Opt a) -> UI t () -> UI t ()
col f ct = UI $ div_ (atts $ f $ Opt [] [Tailwind.flex Col, Tailwind.flex ()]) (fromUI ct)

space :: UI t ()
space = UI $ div_ [classAttribute [Tailwind.grow]] (pure ())



content o = addClass (Tailwind.content o)
self o    = addClass (Tailwind.self o)
items o   = addClass (Tailwind.items o)
basis o   = addClass (Tailwind.basis o)
justify o = addClass (Tailwind.justify o)

w o = addClass (Tailwind.w o)
h o = addClass (Tailwind.h o)

gap o = addClass (Tailwind.gap o)

p  o = addClass (Tailwind.p o)
px o = addClass (Tailwind.px o)
py o = addClass (Tailwind.py o)
pl o = addClass (Tailwind.pl o)
pr o = addClass (Tailwind.pr o)
pt o = addClass (Tailwind.pt o)
pb o = addClass (Tailwind.pb o)

bg o = addClass (Tailwind.bg o)

border o = addClass (Tailwind.border o)

text o = addClass (Tailwind.text o)
font o = addClass (Tailwind.font o)


grow   = addClass Tailwind.grow
shrink = addClass Tailwind.shrink