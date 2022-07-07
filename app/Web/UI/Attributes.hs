module Web.UI.Attributes where

import Prelude hiding ((-))
import Lucid
import Data.Text as Text (Text, intercalate, pack)
import Web.UI.Types

-- | Map classes to a single class attribute. Always
-- append a space in case someone else adds to class
classes :: ToClass c => [c] -> Attribute
classes cls = classNames (map className cls)

classNames :: [Text] -> Attribute
classNames cns = class_ ((Text.intercalate " " cns) <> " ")


data Flex = Row | Col
  deriving (Show, Eq, Enum, Bounded)

flex :: Flex -> Attribute
flex f = classes [f]

bg :: (ToValue color, ToClass color) => color -> Attribute
bg c = classes [BG c]



data BorderColor color = BC color
data BorderWidth = BW Sides BSize


-- TODO
-- I can't specify multiple classes!

-- this is why I need more than one class at a time
border :: (ToValue color, ToClass color) => color -> BSize -> Attribute
border c s = classNames [className (BW All s), className (BC c)]

borderColor :: (ToValue color, ToClass color) => color -> Attribute
borderColor c = classes [BC c]

-- oh no!
-- borderWidth' :: [Sides] -> BSize -> Attribute
-- borderWidth' sds sz = classes [BW (Side L) sz]

borderWidth :: BSize -> Attribute
borderWidth sz = classes [BW All sz]
-- borderWidth = borderWidth' [All]
-- borderX     = borderWidth' [Side L, Side R]
-- borderY     = borderWidth' [Side T, Side B]
-- borderL     = borderWidth' [Side L]
-- borderR     = borderWidth' [Side R]
-- borderT     = borderWidth' [Side T]
-- borderB     = borderWidth' [Side B]











-- TODO custom space sizes
-- how would that work?

data Pad = Pad Sides Space

pad' :: [Sides] -> Space -> Attribute
pad' sds sp = classes $ map (\s -> Pad s sp) sds

pad  = pad' [All]
padX = pad' [Side L, Side R]
padY = pad' [Side T, Side B]
padT = pad' [Side T]
padB = pad' [Side B]
padR = pad' [Side R]
padL = pad' [Side L]




-- I think you only need this one
data Gap s = Gap s

gap :: (ToValue s, ToClass s) => s -> Attribute
gap s = classes [Gap s]











data Background color
  = BG color
  deriving (Show, Eq)




instance ToClass Flex where
  className   Row = "fr"
  className   Col = "fc"

  classStyles Row = ["display:flex", "flex-direction:row"]
  classStyles Col = ["display:flex", "flex-direction:column"]


instance ToClass Pad where
  className   (Pad a size) = "p" <> className a <> className size
  classStyles (Pad All size) =     [ "padding" .: size ]
  classStyles (Pad (Side sd) sz) = [ "padding"-(styleName sd) .: sz ]

instance (ToValue s, ToClass s) => ToClass (Gap s) where
  className   (Gap size) = "g" <> className size
  classStyles (Gap size) = ["gap" .: size]

instance (ToValue color, ToClass color) => ToClass (BorderColor color) where
  className (BC color)     = "bdc" <> className color

  classStyles (BC color)     = ["border-color" .: color]

instance ToClass BorderWidth where
  className (BW side size) = "bdw" <> className side <> className size
  classStyles (BW All size) = ["border-width" .: size]
  classStyles (BW (Side sd) size) = ["border"-(styleName sd)-"width" .: size]

instance (ToValue color, ToClass color) => ToClass (Background color) where
  className (BG color)   = "bgc" <> className color
  classStyles (BG color) = ["background-color" .: color]





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
instance ToClass TRBL

data XY = X | Y
  deriving (Show, Eq, Enum, Bounded)
instance ToClass XY

-- this needs to serialize to *multiple* classes
data Sides
  = Side TRBL
  | All
  deriving (Show)
instance ToClass Sides where
  className (Side s) = className s
  className All      = ""


-- these are always in fixed pixels
data BSize = B0 | B1 | B2 | B4 | B6 | B8
  deriving (Show, Eq, Enum, Bounded)
instance ToClass BSize where
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
instance ToClass Space where
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
