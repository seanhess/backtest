module Tailwind.UI.Classes where

import Prelude
import Tailwind.Options
import Lucid

-- I need a separate types file
import Tailwind (Background, Flex, Border, Gap, Padding, Self, Items, Content, Dimensions)
import Tailwind.Options
import Tailwind.UI.Types
import qualified Tailwind
import qualified Data.Text as Text
import Data.Text (Text)



bg :: Option Background o => o -> Att a
bg o = addClass (Tailwind.bg o)

flex :: Option Flex o => o -> Att a
flex o = addClass (Tailwind.flex o)

border :: Option Border o => o -> Att a
border o = addClass (Tailwind.border o)

gap :: Option Gap o => o -> Att a
gap o = addClass (Tailwind.gap o)

padding :: Option Padding o => o -> Att a
padding o = addClass (Tailwind.padding o)

content :: Option Content o => o -> Att a
content o = addClass (Tailwind.content o)

self :: Option Self o => o -> Att a
self o = addClass (Tailwind.self o)

items :: Option Items o => o -> Att a
items o = addClass (Tailwind.items o)

width :: Option Dimensions o => o -> Att a
width o = addClass (Tailwind.width o)

height :: Option Dimensions o => o -> Att a
height o = addClass (Tailwind.height o)




fromHtml :: (Opt c -> Opt c) -> Html a -> UI a
fromHtml f h = UI $ div_ (atts $ f opt) h

text :: Text -> UI ()
text t = UI $ toHtml t



