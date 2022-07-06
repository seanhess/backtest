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



data Border color
  = BW TRBL BSize
  | BC color
  deriving (Show, Eq)
instance Show color => ClassName (Border color)

-- this is why I need more than one class at a time
border :: (ToValue color, ClassName color) => color -> BSize -> Attribute
border c s = classes [BW T s, BW B s, BW L s, BW R s, BC c]











-- TODO missing custom space
-- TODO specify sides

-- this is the complete range of CLASSES
-- 1. pl, pr, pb, pt
-- 2. p
-- 3. py, px
-- they can control the individual l,r,b,t separately
-- meaning there needs to be a class for each one
data Pad = Pad Sides Space

-- padding n -> p-n
-- padding (Sides n) -> p-n

-- wait, but... we need to do lots of sides
padding :: Space -> Attribute
padding s = classes [Pad All s]

paddingXY :: Space -> Space -> Attribute
paddingXY x y = classes [Pad (Axis X) x, Pad (Axis Y) y]

-- this isn't great. Use a record, or ... anything better
paddingEach :: Space -> Space -> Space -> Space -> Attribute
paddingEach t r b l = classes
  [ Pad (Side T) t
  , Pad (Side R) t
  , Pad (Side B) t
  , Pad (Side L) t
  ] 

data Gap = Gap Space

gap :: Space -> Attribute
gap s = classes [Gap s]











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
  toClass c@(Pad All size)    =
    Class ("p"-(className size))
          [ "padding" .: size ]
  toClass c@(Pad (Axis X) size)    =
    Class ("px"-(className size))
          [ "padding"-(styleName L) .: size
          , "padding"-(styleName R) .: size
          ]
  toClass c@(Pad (Axis Y) size)    =
    Class ("py"-(className size))
          [ "padding"-(styleName T) .: size
          , "padding"-(styleName B) .: size
          ]
  toClass c@(Pad (Side side) size)    =
    Class ("p"-(className side)-(className size))
          ["padding"-(styleName side) .: size]

instance ToClass Gap where
  toClass c@(Gap size)    =
    Class ("g"-(className size))
          ["gap" .: size]

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

data TRBL = T | R | B | L
  deriving (Show, Eq, Enum, Bounded)
instance ToStyle TRBL where
  styleName L = "left"
  styleName R = "right"
  styleName T = "top"
  styleName B = "bottom"
instance ClassName TRBL

data XY = X | Y
  deriving (Show, Eq, Enum, Bounded)
instance ClassName XY

-- this needs to serialize to *multiple* classes
data Sides
  = Side TRBL
  | Axis XY
  | All
  deriving (Show)
instance ClassName Sides where
  className (Side s) = className s
  className (Axis x) = className x
  className All      = ""

-- these are always in fixed pixels
data BSize = B0 | B1 | B2 | B4 | B6 | B8
  deriving (Show, Eq, Enum, Bounded)
instance ClassName BSize where
  className b = pack $ drop 1 $ show b

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
