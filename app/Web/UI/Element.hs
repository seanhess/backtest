module Web.UI.Element where

import Prelude
import Tailwind
import Web.UI.Types
import Data.Text (Text)
import Lucid (toHtml, div_, p_)
import Lucid.Base (Term(..), With(..))
import Data.Default (Default(..))

import Tailwind

-- TODO are we tied to Juniper?

button :: onClick -> UI t () -> UI t ()
button = undefined


-- you have to remember that these are the options
-- not fun!

-- Layout
-- display: flex
-- flex-direction: row | row-reverse | column | column-reverse;
-- flex-wrap: nowrap | wrap | wrap-reverse;
-- justify-content - justify-content: flex-start | flex-end | center | space-between | space-around | space-evenly | start | end | left | right ... + safe | unsafe;
-- align-items: stretch | flex-start | flex-end | center | baseline | first baseline | last baseline | start | end | self-start | self-end + ... safe | unsafe;
-- align-content: flex-start | flex-end | center | space-between | space-around | space-evenly | stretch | start | end | baseline | first baseline | last baseline + ... safe | unsafe;
-- gap (all / row, column)
-- row-gap
-- column-gap

-- Child
-- order: N
-- flex-grow: N
-- flex-shrink: N
-- flex-basis: Width
-- align-self: overrides parent


-- I think this would work...


-- box :: Box -> UI t () -> UI t ()
-- box c ct = UI $ div_ [ classes [ flex c.direction, flex c.wrap ]] (fromUI ct)

-- row :: UI t () -> UI t ()
-- row ct = UI $ div_ [ classes [ flex (), flex Row ] ] (fromUI ct)

-- col :: UI t () -> UI t ()
-- col ct = UI $ div_ [ classes [ flex (), flex Col ] ] (fromUI ct)

-- el :: UI t () -> UI t ()
-- el (UI h) = UI $ div_ h

-- space :: UI t ()
-- space = UI $ div_ [ classes [ grow ] ] $ (pure ())

-- none :: UI t ()
-- none = pure ()

-- Text
-- paragraph :: (Term a res, With res) => a -> res
-- paragraph = p_

-- text :: Text -> UI t ()
-- text t = UI $ toHtml t


-- Table
-- data TableColumn a = TableColumn
--   { header :: UI ()
--   , view :: a -> UI ()
--   }



-- -- these are too vague to extend properly
-- -- this doesn't take any data!
-- table :: (Term a res) => [TableColumn d] -> [d] -> a -> res
-- table _ _ = undefined
--   -- 1. List of data
--   -- 2. Column template
--   -- 3. Column header template?


-- tighter types is more important than that extra []
-- export two of each?
-- an operator that skips them
-- it looks like $



-- example' :: UI ()
-- example' = do

--   -- the other one forces you to put something in
--   -- hmm
--   -- table cols items [borderWidth B0]

--   -- el [] $ do
--   -- el <$ do
--   -- or, one that ADDs them, like we mentioned


--   where
--     cols :: [TableColumn a]
--     cols = undefined

--     items :: [a]
--     items = undefined


-- asdf :: UI ()
-- asdf = do
--   table [] [] $ \d -> do
--     -- these are the rows

