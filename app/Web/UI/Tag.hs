{-# LANGUAGE DeriveAnyClass #-}
module Web.UI.Tag where

import Prelude hiding ((-))
import Control.Monad (forM_)
import Tailwind.Types
import Web.UI.Types
import Web.UI.Attribute (flex, grow)
import Lucid (Html, Attribute, toHtml)
import Lucid.Html5 (button_, div_, input_, placeholder_, value_, select_, option_)
import Data.Text (Text, pack)
import qualified Tailwind.Classes as Tailwind
import qualified Tailwind.Prefix as Prefix
import Tailwind.Values (Direction(Row, Col))
import Juniper (PageAction, onClick, onInput, Value)

-- could have an img, button, etc
-- unless: maybe, we always use a container instead?
el :: Att a -> Html a -> Html a
el f ct = div_ (atts $ f opt) ct

el' :: Html a -> Html a
el' ct = div_ ct

-- | Simple Layout Tools, you can do most LAYOUT with these. Once you get very dynamic
-- you'll need to use grid or flexbox
row :: (Opt a -> Opt a) -> Html a -> Html a
row f = el (flex Row . flex () . f)

col :: (Opt a -> Opt a) -> Html a -> Html a
col f = el (flex Col . flex () . f)

-- can only accept stacked children
-- hmmmm
stack :: (Opt a -> Opt a) -> Html a -> Html a
stack f = el (addClass Tailwind.relative . f)

space :: Html ()
space = el (grow) (pure ())

tag :: ([Attribute] -> Html a -> Html a) -> Att a -> Html a -> Html a
tag tg f ct = tg (atts $ f opt) ct

button :: PageAction action => action -> Att a -> Html a -> Html a
button act f ct = tag button_ (f . addAttribute (onClick act)) ct

-- placeholder
-- value
-- onInout
-- required: onInput, placeholder, default text, 


input :: PageAction act => (Value -> act) -> Text -> Att a -> Html ()
input act val f = input_ ( atts
  . f
  . addAttribute (onInput act)
  . addAttribute (value_ val)
  $ opt)


-- we get a value
-- we need some kind of class instance?
-- it goes back and forth?
-- val -> Text is show equivalent
-- Text -> Maybe val is read equivalent
-- what do we do if we can't read? Up to you

-- that's the Label....
-- hmm...
-- it would be nice if we could do it better
-- TODO we need a better Value system
  
dropdown :: (PageAction act, Show val) => (Value -> act) -> Att a -> [(Text, Text)] -> Html ()
dropdown act toLabel f vals =
  select_ ( atts
    . f
    . addAttribute (onInput act)
  $ opt) $ do
    forM_ vals $ \val -> do
      -- TODO nope, not show
      option_ [value_ (pack $ show val)] (toHtml $ toLabel val)

