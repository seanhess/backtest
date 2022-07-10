{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoOverloadedLists #-}
module Web.UI where

import Prelude hiding ((-), id)
import Tailwind hiding (Black, text)
import qualified Tailwind.Lucid as Tailwind
import Control.Monad.State.Strict (State, StateT, runState, modify, MonadState)
import Tailwind.Options
import qualified Lucid
import Lucid (Attribute, div_, id_)
import Lucid.Base (Term(..), makeElement, With(..))
import Data.List (nub)
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.String (IsString(..))

-- TODO re-export everything, or just use Lucid Attributes for everything
-- either way we will need to re-export all the classes so they write classes


data AppColor
  = Green
  | Black
  | White
instance Segment AppColor where
  seg Green = "green-500"
  seg Black = "black"
  seg White = "white"
instance Option Color AppColor

example :: [Class]
example = [flex Row, bg Green, hover |: font Bold, borderWidth (T B2), borderColor Black, hover |: rotate R90 ]

example' :: Attribute
example' = Tailwind.classes example





newtype UI a = UI { toHtml :: Lucid.Html a }
  deriving (Functor, Applicative, Monad)

instance IsString (UI ()) where
  fromString s = UI $ Lucid.toHtml s

instance Show (UI a) where
  show (UI h) = "UI " <> show h


-- | Given only classes, expect attributes or content
instance (f ~ UI a) => Term [Class] ([Attribute] -> f -> UI a) where
  termWith name atts cs atts2 = with (UI . makeElement name . toHtml) (Tailwind.classes cs : atts <> atts2)

-- | Given classes, expect content as input
instance (f ~ UI a) => Term [Class] (f -> UI a) where
  termWith name atts cs = with (UI . makeElement name . toHtml) (Tailwind.classes cs : atts)

-- | Given children immediately, just use that and expect no attributes
instance Term (UI a) (UI a) where
  termWith name as = with (UI . makeElement name . toHtml ) as
  {-# INLINE termWith #-}

instance With (UI a -> UI a) where
  with f attrs = \ui -> UI $ do
    with (toHtml $ f ui) attrs

instance With ([Attribute] -> UI a -> UI a) where
  with f atts1 = \atts2 ui -> UI $ do
    with (toHtml $ f atts1 ui) atts2

instance With (UI a) where
  with ui = UI . with (toHtml ui)

row :: (Term a result, With result) => a -> result
row a = with (div_ a) [ Tailwind.classes [ flex (), flex Row ] ]

col :: (Term a result, With result) => a -> result
col a = with (div_ a) [ Tailwind.classes [ flex (), flex Col ] ]

el :: (Term a result, With result) => a -> result
el a = div_ a

space :: UI ()
space = el [ grow ] (pure ())

text :: Text -> UI ()
text t = UI $ Lucid.toHtml t

-- turn off overloaded lists, wahoo
test :: UI ()
test = col [bg Green, padding S10, gap S10, inset S1] [id_ "asdf"] $ do
  el [ bg Black ] $ text "hello"
  space
  el (text "nothing")
  space
  el (text "goodbye")

  -- why does this compile?
  -- you can put HTML in here directly
  -- let's call that a feature?
  -- haha
  -- it's making div_ produce a UI somehow
  -- which is funny
  -- div_ [bg Black] [id_ "asdf"] ("this is a normal node")

  "woot"

