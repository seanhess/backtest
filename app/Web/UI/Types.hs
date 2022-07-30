{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.UI.Types where

import Prelude hiding ((-))
import qualified Lucid
import Lucid.Html5 (class_, div_)
import Lucid.Base (Term(..), makeElement, With(..), Attribute, Html)
import Data.String (IsString(..))
import Data.Text as Text (intercalate, Text)
import Web.UI.Size
import qualified Tailwind
-- import Tailwind.Options
-- import Tailwind hiding (Black, text, Segment, Color)

newtype UI t a = UI { fromUI :: Lucid.Html a }
  deriving (Functor, Applicative, Monad)

instance IsString (UI a ()) where
  fromString s = UI $ Lucid.toHtml s

instance Show (UI t a) where
  show (UI h) = "UI " <> show h


newtype Seg a = Seg { fromSeg :: Text }
  deriving (Eq, Show, IsString, Semigroup)

class Segment a where
  seg :: a -> Seg b

instance Segment () where
  seg _ = ""

newtype Class = Class { fromClass :: Text }
  deriving (Show, Eq, IsString)


data Opt a = Opt [Attribute] [[Class]]

opt :: Opt a
opt = Opt [] []

addClass :: [Seg x] -> Opt a -> Opt a
addClass c (Opt as css) = Opt as $ (map toClass c):css

toClass :: Seg x -> Class
toClass (Seg a) = Class a

atts :: Opt a -> [Attribute]
atts (Opt as css) =
  classes css : as

data Parent
data Child

-- data Node a = Node (Html a)
--   deriving Show

-- instance IsString (Node ()) where
--   fromString s = Node $ Lucid.toHtml s

data Node

(-) :: Seg a -> Seg b -> Seg a
a - "" = a
(Seg a) - (Seg b) = Seg $ a <> "-" <> b

-- I could ditch row/col and use
-- it's col by default
-- box, box row, box (row . pad S10), box (col . S12)
-- el, box [ row, ]

-- box :: (Opt Parent -> Opt Parent) -> UI Node a -> UI t a
-- box f ct = UI $ div_ (atts $ f (Opt [] [])) (fromUI ct)

row :: (Opt Parent -> Opt Parent) -> UI [Node] a -> UI t a
row f ct = UI $ div_ (atts $ f (Opt [] [Tailwind.flex Tailwind.Row, flex ()])) (fromUI ct)

col :: (Opt Parent -> Opt Parent) -> UI [Node] a -> UI t a
col f ct = UI $ div_ (atts $ f (Opt [] [Tailwind.flex Tailwind.Col, flex ()])) (fromUI ct)

el :: (Opt Child -> Opt Child) -> UI Node a -> UI [Node] a
el f (UI h) = UI $ div_ (atts $ f (Opt [] [])) h

-- text can be used for either! No options
text :: Text -> UI a ()
text t = UI $ Lucid.toHtml t

-- wait, is button itself a container? No but it could contain one
button :: [[Class]] -> UI Node () -> UI a ()
button css ct = UI $ Lucid.button_ [classes css] (fromUI ct)

space :: UI [Node] ()
space = UI $ div_ [class_ "grow"] (pure ())


class Color c where
  colorName :: c -> String

border :: (Color c) => c -> PixelSize -> Opt Parent -> Opt Parent
border c w = addClass ["border" - show c, "border" - show w]

bg :: Color c => c -> Opt Parent -> Opt Parent
bg c = addClass ["bg" - show c]

gap :: PixelSize -> Opt Parent -> Opt Parent
gap s = addClass ["gap" - show s]


pad :: PixelSize -> Opt Parent -> Opt Parent
pad p = addClass ["p" - show p]

padX :: PixelSize -> Opt Parent -> Opt Parent
padX p = addClass ["px" - show p]

padY :: PixelSize -> Opt Parent -> Opt Parent
padY p = addClass ["py" - show p]

padL :: PixelSize -> Opt Parent -> Opt Parent
padL p = addClass ["pl" - show p]

padR :: PixelSize -> Opt Parent -> Opt Parent
padR p = addClass ["pr" - show p]

padT :: PixelSize -> Opt Parent -> Opt Parent
padT p = addClass ["pt" - show p]

padB :: PixelSize -> Opt Parent -> Opt Parent
padB p = addClass ["pb" - show p]

-- do we want them to be able to specify their own sizing? no
-- you can't set width. Or rather, you can, but it's set
-- this always sets it to a pixel width
-- the option option is grow, you don't say fill?
-- TODO basis fractions

data Length
  = Lpx PixelSize

px :: PixelSize -> Length
px p = Lpx p

width :: Length -> Opt Child -> Opt Child
width l = addClass ["basis" - show l]

-- you can try setting the height but
-- oh shit
-- this doesn't always make sense

height :: Length -> Opt Child -> Opt Child
height l = addClass ["basis" - show l]

grow :: Opt Child -> Opt Child
grow = addClass ["flex-grow"]

-- does align do something different depending on which one it is?
alignItems :: Tailwind.Align -> Opt Parent -> Opt Parent
alignItems a (Opt as css) = Opt as $ Tailwind.flex a : css

align :: Tailwind.Align -> Opt Child -> Opt Child
align a (Opt as css) = Opt as $ Tailwind.self a : css

-- we KNOW that elements are always in a flexbox


-- | Given only classes, expect attributes or content
-- instance (f ~ UI a) => Term [Attribute] ([[Class]] -> f -> UI a) where
--   termWith name as as' cs ct = UI $
--     with (makeElement name (fromUI ct)) (classes cs : as <> as')

-- -- | Given classes, expect content as input
-- instance (f ~ UI a) => Term [[Class]] (f -> UI a) where
--   termWith name as cs ct = UI $
--     with (makeElement name (fromUI ct)) (classes cs : as)

-- | Given children immediately, just use that and expect no attributes
-- instance Term (UI a) (UI a) where
--   -- with :: a -> [attribute] -> a
--   termWith name as ct = UI $
--     with (makeElement name (fromUI ct)) as

-- instance With (UI a -> UI a) where
--   with f attrs = \ui -> UI $ do
--     with (fromUI $ f ui) attrs

-- instance With ([Attribute] -> UI a -> UI a) where
--   with f atts1 = \atts2 ui -> UI $ do
--     with (fromUI $ f atts1 ui) atts2

-- instance With ([[Class]] -> UI a -> UI a) where
--   with f atts1 = \cs ui -> UI $ do
--     with (fromUI $ f cs ui) atts1


-- | Convert classes into an Attribute
classes :: [[Class]] -> Attribute
classes cns = class_ (Text.intercalate " " (map fromClass (mconcat cns)) <> " ")






-- class AddAttributes f where
--   (!) :: ToAttributes a => f -> a -> f

-- instance AddAttributes (UI t () -> UI t ()) where
--   uf ! a = \ui -> ui ! a

-- instance AddAttributes (UI t ()) where
--   (UI h) ! a = UI $ with h (attributes a)


-- class ToAttributes a where
--   attributes :: a -> [Attribute]

-- instance ToAttributes Attribute where
--   attributes a = [a]

-- instance ToAttributes () where
--   attributes a = []

-- instance ToAttributes [Attribute] where
--   attributes a = a

-- instance (ToAttributes a, ToAttributes b) => ToAttributes (a, b) where
--   attributes (a, b) = concat [attributes a, attributes b]

-- instance (ToAttributes a, ToAttributes b, ToAttributes c) => ToAttributes (a, b, c) where
--   attributes (a, b, c) = concat [attributes a, attributes b, attributes c]

-- instance (ToAttributes a, ToAttributes b, ToAttributes c, ToAttributes d) => ToAttributes (a, b, c, d) where
--   attributes (a, b, c, d) = concat [attributes a, attributes b, attributes c, attributes d]

-- instance (ToAttributes a, ToAttributes b, ToAttributes c, ToAttributes d, ToAttributes e) => ToAttributes (a, b, c, d, e) where
--   attributes (a, b, c, d, e) = concat [attributes a, attributes b, attributes c, attributes d, attributes e]

-- instance ToAttributes [[Class]] where
--   attributes css = [classes css]

-- instance ToAttributes [Class] where
--   attributes css = [classes [css]]