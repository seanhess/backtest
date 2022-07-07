module Web.UI.Attributes where

import Prelude hiding ((-))
import Lucid
import Data.Text as Text (Text, intercalate, pack)
import Web.UI.Types

-- | Map classes to a single class attribute. Always
-- append a space in case someone else adds to class
classes :: Class c => [c] -> Attribute
classes cls = classNames (map toClass cls)

classNames :: [Class_] -> Attribute
classNames cns = class_ ((Text.intercalate " " (map className cns)) <> " ")


data Flex = Row | Col
  deriving (Show, Eq, Enum, Bounded)

flex :: Flex -> Attribute
flex f = classes [f]


bg :: (Value color) => color -> Attribute
bg c = classes [BG c]



-- TODO I wish these could be one unified datatype
data BorderColor color = BC color
data BorderWidth = BW Sides BSize



-- TODO having to collapse to classNames isn't awesome

border :: (Value color) => color -> BSize -> Attribute
border c s = classNames [toClass (BW All s), toClass (BC c)]

borderColor :: (Value color, Class color) => color -> Attribute
borderColor c = classes [BC c]

borderWidth' :: [Sides] -> BSize -> Attribute
borderWidth' sds sz = classes [BW (Side L) sz]

borderWidth = borderWidth' [All]
borderX     = borderWidth' [Side L, Side R]
borderY     = borderWidth' [Side T, Side B]
borderL     = borderWidth' [Side L]
borderR     = borderWidth' [Side R]
borderT     = borderWidth' [Side T]
borderB     = borderWidth' [Side B]











data Pad space = Pad Sides space

pad' :: (Value space) => [Sides] -> space -> Attribute
pad' sds sp = classes $ map (\s -> Pad s sp) sds

pad :: (Value space) => space -> Attribute
pad  = pad' [All]

padX :: (Value space) => space -> Attribute
padX = pad' [Side L, Side R]

padY :: (Value space) => space -> Attribute
padY = pad' [Side T, Side B]

padT :: (Value space) => space -> Attribute
padT = pad' [Side T]

padB :: (Value space) => space -> Attribute
padB = pad' [Side B]

padR :: (Value space) => space -> Attribute
padR = pad' [Side R]

padL :: (Value space) => space -> Attribute
padL = pad' [Side L]




data Gap s = Gap s

gap :: (Value s) => s -> Attribute
gap s = classes [Gap s]


data AppSpace
  = AS1
  | AS2
  | AS3
  deriving (Show)
instance Segment AppSpace

instance Value AppSpace where
  units AS1 = (Rem 0.345)
  units AS2 = (Rem 0.800)
  units AS3 = (Rem 1.9)



test :: [Attribute]
test = [gap AS1]









data Background color
  = BG color
  deriving (Show, Eq)




instance Class Flex where
  toClass Row = Class "fr" ["display:flex", "flex-direction:row"]
  toClass Col = Class "fc" ["display:flex", "flex-direction:column"]


instance (Value space) => Class (Pad space) where
  toClass (Pad a size) =
    Class ("p" <> segment a - segment size) (styles a)
    where
      styles All       = [ "padding" .: size ]
      styles (Side sd) = [ "padding"-(sideName sd) .: size ]


instance (Value space) => Class (Gap space) where
  toClass (Gap size) = 
    Class ("g" - segment size)
          ["gap" .: size]

instance (Value color) => Class (BorderColor color) where
  toClass (BC color) =
    Class ("bdc" - segment color)
          ["border-color" .: color]

instance Class BorderWidth where
  toClass (BW side size) =
    Class ("bdw" <> segment side - segment size) (styles side)
    where
      styles All       = ["border-width" .: size]
      styles (Side sd) = ["border"-(sideName sd)-"width" .: size]

instance (Value color) => Class (Background color) where
  toClass (BG color) =
    Class ("bgc" - segment color)
          ["background-color" .: color]





-- VALUES ----
-----------------------------------

-- put these all together

data TRBL = T | R | B | L
  deriving (Show, Eq, Enum, Bounded)
instance Segment TRBL

-- is this a class?
sideName :: TRBL -> Text
sideName L = "left"
sideName R = "right"
sideName T = "top"
sideName B = "bottom"

data XY = X | Y
  deriving (Show, Eq, Enum, Bounded)
instance Segment XY

-- this needs to serialize to *multiple* classes
data Sides
  = Side TRBL
  | All
  deriving (Show)
instance Segment Sides where
  segment (Side s) = segment s
  segment All      = ""


-- these are always in fixed pixels
data BSize = B0 | B1 | B2 | B4 | B6 | B8
  deriving (Show, Eq, Enum, Bounded)
instance Segment BSize where
  segment b = pack $ drop 1 $ show b

instance Value BSize where
  units B0 = Px 0
  units B1 = Px 1
  units B2 = Px 2
  units B4 = Px 4
  units B6 = Px 6
  units B8 = Px 8


-- this is the default
-- you can extend it by making your own
data Space = S0 | S1 | S2 | S4 | S6 | S8 | S10 | S12 | S14 | S16 | S20 | S24 | S28 | S32 | S36 | S40 | S44 | S48
  deriving (Show, Eq, Enum, Bounded)
instance Segment Space where
  segment s = pack $ drop 1 $ show s
instance Value Space where
  units S0 = Px 0
  units S1 = Px 1
  units S2 = Rem 0.125
  units S4 = Rem 0.25
  units S6 = Rem 0.375
  units S8 = Rem 0.5
  units S10 = Rem 0.625
  units S12 = Rem 0.75
  units S14 = Rem 0.875
  units S16 = Rem 1
  units S20 = Rem 1.25
  units S24 = Rem 1.5
  units S28 = Rem 1.75
  units S32 = Rem 2
  units S36 = Rem 2.25
  units S40 = Rem 2.5
  units S44 = Rem 2.75
  units S48 = Rem 3
