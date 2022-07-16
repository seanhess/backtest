{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.UI.Types where

import Prelude
import qualified Lucid
import Lucid.Html5 (class_, div_)
import Lucid.Base (Term(..), makeElement, With(..), Attribute, Html)
import Data.String (IsString(..))
import Data.Text as Text (intercalate, Text)
import Tailwind.Options
import Tailwind hiding (Black, text)


newtype UI t a = UI { fromUI :: Lucid.Html a }
  deriving (Functor, Applicative, Monad)

instance IsString (UI a ()) where
  fromString s = UI $ Lucid.toHtml s

instance Show (UI t a) where
  show (UI h) = "UI " <> show h



data Opt a = Opt [Attribute] [[Class]]

opt :: Opt a
opt = Opt [] []

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

row :: (Opt Parent -> Opt Parent) -> UI [Node] a -> UI Node a
row f ct = UI $ div_ (atts $ f (Opt [] [flex Row, flex ()])) (fromUI ct)

col :: (Opt Parent -> Opt Parent) -> UI [Node] a -> UI Node a
col f ct = UI $ div_ (atts $ f (Opt [] [flex Col, flex ()])) (fromUI ct)

el :: (Opt Child -> Opt Child) -> UI Node a -> UI [Node] a
el f (UI h) = UI $ div_ (atts $ f (Opt [] [])) h

-- text can be used for either! No options
text :: Text -> UI a ()
text t = UI $ Lucid.toHtml t




background :: Option Color c => c -> Opt Parent -> Opt Parent
background c (Opt as css) = Opt as $ bg c : css

gap :: Option Gap s => s -> Opt Parent -> Opt Parent
gap g (Opt as css) = Opt as $ Tailwind.gap g : css

padding :: Option Padding s => s -> Opt Parent -> Opt Parent
padding g (Opt as css) = Opt as $ Tailwind.padding g : css

width :: Option Dimensions s => s -> Opt Child -> Opt Child
width s (Opt as css) = Opt as $ Tailwind.basis s : css

-- does align do something different depending on which one it is?
alignItems :: Align -> Opt Parent -> Opt Parent
alignItems a (Opt as css) = Opt as $ flex a : css

align :: Align -> Opt Child -> Opt Child
align a (Opt as css) = Opt as $ self a : css

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