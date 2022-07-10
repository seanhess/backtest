{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.UI where

import Prelude hiding ((-))
import Tailwind hiding (Black, text)
import qualified Tailwind.Lucid as Tailwind
import Control.Monad.State.Strict (State, StateT, runState, modify, MonadState)
import Tailwind.Options
import Lucid
import Lucid.Base (Term(..), makeElement)
import Data.List (nub)
import Data.Text (Text)
import Data.Maybe (mapMaybe)


data AppColor
  = Green
  | Black
  | White
instance Segment AppColor where
  seg Green = "green-500"
  seg Black = "black"
  seg White = "white"
instance Option Color AppColor

example :: [[Class]]
example = [flex Row, bg Green, hover |: font Bold, borderWidth (T B2), borderColor Black, hover |: rotate R90 ]

example' :: Attribute
example' = Tailwind.classes example





data Att
  = Att Attribute
  | Classes [Class]

toAttributes :: [Att] -> [Attribute]
toAttributes ats =
  let css = mapMaybe getClass ats
      as = mapMaybe getAtt ats
  in Tailwind.classes css : as
  where
    getClass (Classes cs) = Just cs
    getClass _ = Nothing

    getAtt (Att a) = Just a
    getAtt _ = Nothing

newtype UI a = UI { fromUI :: Html a }
  deriving (Functor, Applicative, Monad)

-- | Given attributes, expect more child input.
instance (f ~ UI a) => Term [Att] (f -> UI a) where
  termWith name f = with (UI . makeElement name . fromUI ) . (\as -> toAttributes $ (as <> map Att f) )

-- | Given children immediately, just use that and expect no attributes
instance Term (UI a) (UI a) where
  termWith name as = with (UI . makeElement name . fromUI ) as
  {-# INLINE termWith #-}

instance With (UI a -> UI a) where
  with f attrs = \ui -> UI $ do
    with (fromUI $ f ui) attrs

instance With (UI a) where
  with ui = UI . with (fromUI ui)

--  with f = \attr -> HtmlT (mk attr <$> runHtmlT (f inner))
--    where
--      mk attr ~(f',a) = (\attr' -> f' (unionArgs (M.fromListWith (<>) (map toPair attr)) attr'),a)
--      toPair (Attribute x y) = (x,y)


-- nope this requires actual attributes
col :: (Term arg result, With result) => arg -> result
col arg = with (term "div" arg) [ Tailwind.classes [ flex Col ] ]

el :: (Term arg result, With result) => arg -> result
el arg = with (div_ arg) []

space :: UI ()
space = el [ Classes grow ] (pure ())

text :: Text -> UI ()
text t = UI $ toHtml t

test :: UI ()
test = col [ Classes (bg Green), Classes (padding S10), Classes (gap S10), Att (id_ "asdf") ] $ do
  el [ Classes (bg Black) ] $ text "hello"
  space
  el (text "nothing")
  space
  el [ Classes (bg White) ] (text "goodbye")

-- node :: ([Attribute] -> Html () -> Html ()) -> term -> UI ()
-- node n =

-- div = node div_