module Tailwind.UI.Classes where

import Prelude hiding (div)
import Tailwind.Options
import Lucid

-- I need a separate types file
import Tailwind (Background, Flex, Border, Gap, Padding, Self, Items, Content, Dimensions, Font, FontText)
import Tailwind.Options
import Tailwind.UI.Types
import qualified Tailwind
import qualified Data.Text as Text
import Data.Text (Text)



-- the layout ones ONLY take layout
-- export div, el?

data Node

-- you can do MOST things, right?
-- it's just some aren't allowed in layout
-- can we make it stricter?
bg :: Option Background o => o -> Att a
bg o = addClass (Tailwind.bg o)

border :: Option Border o => o -> Att a
border o = addClass (Tailwind.border o)

text :: Option FontText o => o -> Att a
text o = addClass (Tailwind.text o)

font :: Option Font o => o -> Att a
font o = addClass (Tailwind.font o)




-- oh, but this can do anything, including layout
-- flex :: Option Flex o => o -> Att Node
-- flex o = addClass (Tailwind.flex o)






