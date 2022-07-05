module Web.UI.Classes where

import Prelude hiding ((-))
import Lucid
import Data.Text as Text (Text, intercalate)
import Web.UI.Types


-- they all have to be equal
-- class0 and class1
-- if they are in the same array :(
classes :: ToClass c => [c] -> Attribute
classes cls = class_ ((Text.intercalate " " $ map (name . toClass) cls) <> " ")

-- flex :: Flex -> Attribute
-- flex f = classes [flex' f]


-- it creates a ... what?
-- a thing you can do!



-- flex :: Flex -> a

-- flex :: Flex -> Class
-- flex Row = Class "fr" ["display:flex", "flex-direction:row"]
-- flex Col = Class "fc" ["display:flex", "flex-direction:column"]

-- background :: (ClassName color, ToValue color) => color -> Class
-- background c = Class ("bg"-className c) ["background-color" .: c]

-- umm... that's pretty simple
bg :: color -> Class1 color
bg = BG

-- doesn't depend on color. Can be embedded directly into things?
data Class0
  = Flex Flex
  deriving (Show, Eq)

instance ClassName Class0

instance ToAttribute Class0 where
  toAttribute c = classes [c]


data Class1 color
  = BW Sides BSize
  | BC color
  | BG color
  deriving (Show, Eq)

instance Show color => ClassName (Class1 color)

instance (ToValue color, ClassName color) => ToAttribute (Class1 color) where
  toAttribute c = classes [c]



class ToClass a where
  toClass :: a -> Class

instance ToClass Class0 where
  toClass c@(Flex Row) = Class "fr" ["display:flex", "flex-direction:row"]
  toClass c@(Flex Col) = Class "fc" ["display:flex", "flex-direction:column"]

instance (ToValue color, ClassName color) => ToClass (Class1 color) where
  toClass c@(BW side size) =
    Class ("bdw"-(className side)-(className size))
          ["border"-(styleName side)-"width" .: size]

  toClass c@(BC color) =
    Class ("bdc"-className color)
          ["border-color" .: color]

  toClass c@(BG color) =
    Class ("bgc"-className color) ["background-color" .: color]





-- VALUES ----
-----------------------------------

data Flex = Row | Col
  deriving (Show, Eq, Enum, Bounded)

-- put these all together

data Sides = L | R | T | B
  deriving (Show, Eq, Enum, Bounded)
instance ToStyle Sides where
  styleName L = "left"
  styleName R = "right"
  styleName T = "top"
  styleName B = "bottom"
instance ClassName Sides

data BSize = B0 | B1 | B2 | B4 | B6 | B8
  deriving (Show, Eq, Enum, Bounded)
instance ClassName BSize

instance ToValue BSize where
  value B0 = Px 0
  value B1 = Px 1
  value B2 = Px 2
  value B4 = Px 4
  value B6 = Px 6
  value B8 = Px 8
