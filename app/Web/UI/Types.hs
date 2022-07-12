{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.UI.Types where

import Prelude
import qualified Lucid
import Lucid.Html5 (class_)
import Lucid.Base (Term(..), makeElement, With(..), Attribute)
import Data.String (IsString(..))
import Data.Text as Text (intercalate)
import Tailwind.Options
import Tailwind hiding (Black, text)

newtype UI a = UI { fromUI :: Lucid.Html a }
  deriving (Functor, Applicative, Monad)

instance IsString (UI ()) where
  fromString s = UI $ Lucid.toHtml s

instance Show (UI a) where
  show (UI h) = "UI " <> show h


-- | Given only classes, expect attributes or content
instance (f ~ UI a) => Term [Attribute] ([Class] -> f -> UI a) where
  termWith name atts atts2 cs = with (UI . makeElement name . fromUI) (classes cs : atts <> atts2)

-- | Given classes, expect content as input
instance (f ~ UI a) => Term [Class] (f -> UI a) where
  termWith name atts cs = with (UI . makeElement name . fromUI) (classes cs : atts)

-- | Given children immediately, just use that and expect no attributes
instance Term (UI a) (UI a) where
  termWith name as = with (UI . makeElement name . fromUI ) as
  {-# INLINE termWith #-}

instance With (UI a -> UI a) where
  with f attrs = \ui -> UI $ do
    with (fromUI $ f ui) attrs

instance With ([Attribute] -> UI a -> UI a) where
  with f atts1 = \atts2 ui -> UI $ do
    with (fromUI $ f atts1 ui) atts2

instance With ([Class] -> UI a -> UI a) where
  with f atts1 = \cs ui -> UI $ do
    with (fromUI $ f cs ui) atts1

instance With (UI a) where
  with ui = UI . with (fromUI ui)


-- | Convert classes into an Attribute
classes :: [Class] -> Attribute
classes cns = class_ (Text.intercalate " " (map fromClass cns) <> " ")


