module Web.UI.Classes where

import Prelude hiding ((-))
import Lucid
import Data.Text as Text (Text, intercalate)
import Web.UI.Types


-- classes :: [Class'] -> Attribute
-- classes cls = class_ ((Text.intercalate " " $ map name cls) <> " ")

-- flex :: Flex -> Attribute
-- flex f = classes [flex' f]

flex :: Flex -> Class
flex Row = Class "fr" ["display:flex", "flex-direction:row"]
flex Col = Class "fc" ["display:flex", "flex-direction:column"]

background :: (ClassName color, ToValue color) => color -> Class
background c = Class ("bg"-className c) ["background-color" .: c]

-- what about the side?
borderWidthX :: BSize -> Class

borderWidthX s = Class ("bwl"-)
  where
    bwl = "bwl"



-- is this a class?
-- we could have non-unified types?
-- the problem is specifying a color
data Border color
  = BorderWidth Sides BSize
  | BorderColor color

-- it doesn't have color in it
-- maybe I'm not providing very many things like row/col
-- maybe I want to go back to that
-- el [ row ] isn't so bad

data FlexClass
  = Flex Flex

-- doesn't help

-- Need to spend some time drawing and thinking


data Flex = Row | Col
  deriving (Show, Eq, Enum, Bounded)

data Sides = L | R | T | B
  deriving (Show, Eq, Enum, Bounded)
instance ToStyle Sides where
  styleName L = "left"
  styleName R = "right"
  styleName T = "top"
  styleName B = "bottom"

data BSize = B0 | B1 | B2 | B4 | B6 | B8
  deriving (Show, Eq, Enum, Bounded)

instance ToValue BSize where
  value B0 = Px 0
  value B1 = Px 1
  value B2 = Px 2
  value B4 = Px 4
  value B6 = Px 6
  value B8 = Px 8