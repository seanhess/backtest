{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoOverloadedLists #-}
module Web.UI where

import Prelude hiding ((-), id)
import Tailwind hiding (Black, text)
import qualified Tailwind.Lucid as Tailwind
import Control.Monad.State.Strict (State, StateT, runState, modify, MonadState)
import Tailwind.Options
import Lucid
import Lucid.Base (Term(..), makeElement)
import Data.List (nub)
import Data.Text (Text)
import Data.Maybe (mapMaybe)

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





-- data Att
--   = Att Attribute
--   | Classes [Class]

-- toAttributes :: [Att] -> [Attribute]
-- toAttributes ats =
--   let css = mapMaybe getClass ats
--       as = mapMaybe getAtt ats
--   in Tailwind.classes (mconcat css) : as
--   where
--     getClass (Classes cs) = Just cs
--     getClass _ = Nothing

--     getAtt (Att a) = Just a
--     getAtt _ = Nothing

newtype UI a = UI { fromUI :: Html a }
  deriving (Functor, Applicative, Monad)

instance Show (UI a) where
  show (UI h) = "UI " <> show h

-- class El a where
--   element :: a -> UI ()



-- instance El (UI ()) where
--   element a = a

-- instance El ([Class] -> UI ()) where
--   element _ ui = ui

-- | Given only classes, expect attributes or content
instance (f ~ UI a) => Term [Class] ([Attribute] -> f -> UI a) where
  termWith name atts cs atts2 = with (UI . makeElement name . fromUI) (Tailwind.classes cs : atts <> atts2)

-- | Given classes, expect content as input
instance (f ~ UI a) => Term [Class] (f -> UI a) where
  termWith name atts cs = with (UI . makeElement name . fromUI) (Tailwind.classes cs : atts)

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

instance With (UI a) where
  with ui = UI . with (fromUI ui)



-- nope this requires actual attributes
col :: (Term arg result, With result) => arg -> result
col arg = with (term "div" arg) [ Tailwind.classes [ flex Col ] ]

el :: (Term arg result, With result) => arg -> result
el arg = with (div_ arg) []

space :: UI ()
space = el [ grow ] (pure ())

text :: Text -> UI ()
text t = UI $ toHtml t

-- turn off overloaded lists, wahoo
test :: UI ()
test = col [bg Green, padding S10, gap S10] [id_ "asdf"] $ do
  el [ bg Black ] $ text "hello"
  space
  el (text "nothing")
  space
  el (text "goodbye")
  text "woot"




-- oh, neat just allow classes in a separate list!


-- node :: ([Attribute] -> Html () -> Html ()) -> term -> UI ()
-- node n =

-- div = node div_