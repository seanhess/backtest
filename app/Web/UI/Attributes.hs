module Web.UI.Attributes where

import Prelude hiding ((-))
import Lucid
import Data.Text as Text (Text, intercalate, pack)
import Web.UI.Types

-- | Map classes to a single class attribute. Always
-- append a space in case someone else adds to class
classes :: ToClass c => [c] -> Attribute
classes cls = class_ ((Text.intercalate " " $ map (name . toClass) cls) <> " ")


data Flex = Row | Col
  deriving (Show, Eq, Enum, Bounded)

flex :: Flex -> Attribute
flex f = classes [f]

bg :: (ToValue color, ClassName color) => color -> Attribute
bg c = classes [BG c]


-- these don't need to be exposed to the user
-- do I need them at all any more?
-- I need a way to generate the range of values
-- it could still be a function though
-- no, we don't want 2 kinds of functions
data Border color
  = BW Sides BSize
  | BC color
  deriving (Show, Eq)
instance Show color => ClassName (Border color)

-- this is why I need more than one class at a time
border :: (ToValue color, ClassName color) => color -> BSize -> Attribute
border c s = classes [BW T s, BW B s, BW L s, BW R s, BC c]






data Pad = Pad Sides Space

padding :: Space -> Attribute
padding s = classes [Pad L s, Pad R s, Pad T s, Pad B s]





data Background color
  = BG color
  deriving (Show, Eq)
instance Show color => ClassName (Background color)




class ToClass a where
  toClass :: a -> Class

instance ToClass Flex where
  toClass Row = Class "fr" ["display:flex", "flex-direction:row"]
  toClass Col = Class "fc" ["display:flex", "flex-direction:column"]

instance ToClass Pad where
  toClass c@(Pad side size)    =
    Class ("p"-(className side)-(className size))
          ["padding" .: size]

instance (ToValue color, ClassName color) => ToClass (Border color) where
  toClass c@(BW side size) =
    Class ("bdw"-(className side)-(className size))
          ["border"-(styleName side)-"width" .: size]

  toClass c@(BC color) =
    Class ("bdc"-className color)
          ["border-color" .: color]

instance (ToValue color, ClassName color) => ToClass (Background color) where
  toClass c@(BG color) =
    Class ("bgc"-className color) ["background-color" .: color]





-- VALUES ----
-----------------------------------

-- put these all together

data Sides = L | R | T | B
  deriving (Show, Eq, Enum, Bounded)
instance ToStyle Sides where
  styleName L = "left"
  styleName R = "right"
  styleName T = "top"
  styleName B = "bottom"
instance ClassName Sides


-- these are always in fixed pixels
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


-- this is the default
-- you can extend it by making your own
data Space = S0 | S1 | S2 | S4 | S6 | S8 | S10 | S12 | S14 | S16 | S20 | S24 | S28 | S32 | S36 | S40 | S44 | S48
  deriving (Show, Eq, Enum, Bounded)
instance ClassName Space where
  className s = pack $ drop 1 $ show s
instance ToValue Space where
  value S0 = Px 0
  value S1 = Px 1
  value S2 = Rem 0.125
  value S4 = Rem 0.25
  value S6 = Rem 0.375
  value S8 = Rem 0.5
  value S10 = Rem 0.625
  value S12 = Rem 0.75
  value S14 = Rem 0.875
  value S16 = Rem 1
  value S20 = Rem 1.25
  value S24 = Rem 1.5
  value S28 = Rem 1.75
  value S32 = Rem 2
  value S36 = Rem 2.25
  value S40 = Rem 2.5
  value S44 = Rem 2.75
  value S48 = Rem 3

data XY a = XY
  { x :: a
  , y :: a
  }

data Sides' a = Sides'
  { left :: a
  , right :: a
  }