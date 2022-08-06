module Tailwind.UI.Layout where

import Prelude
import Tailwind (Direction(..))
import qualified Tailwind
import Lucid (div_)
import Tailwind.UI.Types
import Tailwind.UI.Classes
import Tailwind.Options
import Tailwind (Gap, Padding, Self, Items, Content, Dimensions)



-- TODO this won't be included!
row :: (Opt a -> Opt a) -> UI t () -> UI t ()
row f ct = UI $ div_ (atts $ f $ Opt [] [Tailwind.flex Row, Tailwind.flex ()]) (fromUI ct)

col :: (Opt a -> Opt a) -> UI t () -> UI t ()
col f ct = UI $ div_ (atts $ f $ Opt [] [Tailwind.flex Col, Tailwind.flex ()]) (fromUI ct)

space :: UI t ()
space = UI $ div_ [classAttribute [Tailwind.grow]] (pure ())


gap :: Option Gap o => o -> Att a
gap o = addClass (Tailwind.gap o)

p :: Option Padding o => o -> Att a
p o = addClass (Tailwind.p o)

-- we DON'T want to remember these options. Too much flex knowledge
-- what if we require the gap/pad?
content :: Option Content o => o -> Att a
content o = addClass (Tailwind.content o)

self :: Option Self o => o -> Att a
self o = addClass (Tailwind.self o)

items :: Option Items o => o -> Att a
items o = addClass (Tailwind.items o)

w :: Option Dimensions o => o -> Att a
w o = addClass (Tailwind.w o)

h :: Option Dimensions o => o -> Att a
h o = addClass (Tailwind.h o)


-- 1. Wrapping
-- 2. Space around? I think you can do it with padding just fine
-- I need to play with this, create an actual html file

-- hmmm
-- I want to be more specific about what a layout expects, about what is possible
-- right now, they are nodes and you coulld put ANYTHING in them
-- but really, I want to ask for layout options ONLY, then anything like normal
-- and I want a limited number of layout options

-- layout?
-- his options are incomplete
-- you can't do EVERYTHING with this, can you?
-- space is a huge adjustment, if you can make things fill space, you can accomplish a lot
