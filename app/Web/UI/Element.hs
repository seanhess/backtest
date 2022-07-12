module Web.UI.Element where

import Prelude
import Tailwind
import Web.UI.Types
import Data.Text (Text)
import Lucid (toHtml, div_, p_)
import Lucid.Base (Term, With(..))

-- Layout
row :: (Term a res, With res) => a -> res
row a = with (div_ a) [ classes [ flex (), flex Row ] ]

col :: (Term a res, With res) => a -> res
col a = with (div_ a) [ classes [ flex (), flex Col ] ]

el :: (Term a res, With res) => a -> res
el a = with (div_ a) []

space :: UI ()
space = el [ grow ] (pure ())


none :: UI ()
none = pure ()

-- Text
paragraph :: (Term a res, With res) => a -> res
paragraph = p_

text :: Text -> UI ()
text t = UI $ toHtml t


-- Table

-- this makes it hard to accept exact children, no?
-- but maybe the table shouldn't be allowed to have any styles on it directly
-- it's always 100% width, so put it inside of something
-- can you put ANYTHING inside of rows and cols?
-- or only other layout things?

table :: (Term a res, With res) => a -> res
table = undefined -- TODO
  -- 1. List of data
  -- 2. Column template
  -- 3. Column header template?


-- asdf :: UI ()
-- asdf = do
--   table [] [] $ \d -> do
--     -- these are the rows

