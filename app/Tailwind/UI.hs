{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tailwind.UI
  ( UI(..)
  , row, col, el, fromHtml, text
  , bg, flex
  , border
  , module Tailwind.Options

  ) where

import Prelude
import Tailwind.Options
import Lucid

-- I need a separate types file
import Tailwind (Background, Flex, Border, Gap, Padding)
import Tailwind.Options
import qualified Tailwind
import qualified Data.Text as Text
import Data.Text (Text)
import Data.String (IsString(..))


type Att a = Opt a -> Opt a

bg :: Option Background o => o -> Att a
bg o = addClass (Tailwind.bg o)

flex :: Option Flex o => o -> Att a
flex o = addClass (Tailwind.flex o)

border :: Option Border o => o -> Att a
border o = addClass (Tailwind.border o)

gap :: Option Gap o => o -> Att a
gap o = addClass (Tailwind.gap o)

padding :: Option Padding o => o -> Att a
padding o = addClass (Tailwind.padding o)






-- layout has stricter rules
-- row, col, gap, pad, etc
-- then you switch into normal mode
-- it doesn't let you put whatever you want in it, only layout commands

row :: (Opt p -> Opt p) -> UI a -> UI a
row f ct = UI $ div_ (atts $ f . flex Row . flex () $ opt) (fromUI ct)

col :: (Opt p -> Opt p) -> UI a -> UI a
col f ct = UI $ div_ (atts $ f . flex Col . flex () $ opt) (fromUI ct)

el :: (Opt c -> Opt c) -> UI a -> UI a
el f ct = UI $ div_ (atts $ f opt) (fromUI ct)

fromHtml :: (Opt c -> Opt c) -> Html a -> UI a
fromHtml f h = UI $ div_ (atts $ f opt) h

text :: Text -> UI ()
text t = UI $ toHtml t

-- | UI instead of HTML

newtype UI a = UI { fromUI :: Lucid.Html a }
  deriving (Functor, Applicative, Monad)

instance IsString (UI ()) where
  fromString s = UI $ Lucid.toHtml s

instance Show (UI a) where
  show (UI h) = "UI " <> show h



data Opt a = Opt [Attribute] [[Class]]

opt :: Opt a
opt = Opt [] []

addClass :: [Class] -> Opt a -> Opt a
addClass c (Opt as css) = Opt as $ c:css

atts :: Opt a -> [Attribute]
atts (Opt as css) =
  classAttribute css : as

-- | Convert classes into an Attribute
classAttribute :: [[Class]] -> Attribute
classAttribute cns = class_ (Text.intercalate " " (map fromClass (mconcat cns)) <> " ")