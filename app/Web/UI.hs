{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.UI where

import Prelude hiding ((-))
import Tailwind hiding (Black, text)
import qualified Tailwind.Lucid as Tailwind
import Control.Monad.State.Strict (State, StateT, runState, modify, MonadState)
import Tailwind.Options
import Lucid
import Data.List (nub)
import Data.Text (Text)


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





data Node = Node
  { _attributes :: [Attribute]
  , _classes :: [[Class]]
  , _content :: UI ()
  -- , _node :: [Attribute] -> Html () -> Html ()
  }

empty :: Node
empty = Node [] [] (pure ())

newtype UI a = UI { fromUI :: State Node a }
  deriving (Functor, Applicative, Monad, MonadState Node)

runUI :: forall a. ([Attribute] -> Html () -> Html ()) -> UI a -> Html a
runUI html (UI st) = do
  let (a, n) = runState st empty :: (a, Node)

  -- wait, where DO I set what the node is though?
  -- Argh
  -- I... don't? the parent does?
  -- erm
  -- col ... does what exactly
  html (allAtts n) (runUI )
  -- to run, you must start with a node
  -- (_node n) (allAtts n) (runUI (_content n))
  pure a
  where
    allAtts :: Node -> [Attribute]
    allAtts n = (Tailwind.classes (_classes n)) : (_attributes n)

-- fromHtml :: Html () -> UI ()
-- fromHtml = 

-- runs a ui and turns it into something that can be combined
node :: ([Attribute] -> Html () -> Html ()) -> UI () -> UI ()
node f ui = _ -- modify $ \n -> n { _node = f }

classes :: [[Class]] -> UI ()
classes css =
  modify (\n -> n { _classes = nub (css <> (_classes n)) })

content :: UI () -> UI ()
content ct = 
  modify $ \n -> n { _content = ct }

-- we want to set the children?
-- yeah, anything you set is a child
-- wait.... no?
-- I'm confused :)
col :: UI () -> UI ()
col ct = do
  classes [flex Col]
  -- node div_

  -- this is my childrenz
  content ct


-- -- usage: 
-- addStuff :: Attribute -> UI ()
-- addStuff a = UI f
--   where
--     f as = html

-- we want to be able to set attributes INSIDE
-- wait, what would this mean:
-- it's...
-- it doesn't have a node name
-- it's weird
-- why does it work for html??
-- I can still create an html
-- it's just a
-- we are building content
-- but what's my node?
-- we don't know
-- so it's not in there

text :: Text -> UI ()
text t = _

test' :: UI ()
test' = do
  classes [bg Green]
  text "hello"

test :: UI ()
test = col $ do
  -- attributes [ id_ "asdf" ]
  -- each of these should set the PARENT classes
  classes [bg Green]

  -- each of these should set the content
  text "test"



-- row :: UI a -> UI a
-- row = _

-- col :: UI a -> UI a
-- col = _

-- classes :: [[Class]] -> UI a
-- classes = _

-- text :: Text -> UI a
-- text = _

-- attributes :: [Attribute] -> UI a
-- attributes = _

-- test :: Html ()
-- test = div_ "hello"


-- let's experiment!






-- we definitely don't want to do our own monad
-- we want to piggy back on html

-- well, it actually builds an html, right?
-- ok! how do I make this a monad?
-- it's basically a state monad, right?
-- but... it's a deeply nested structure
-- so.... 
-- hm...
-- yeah, state could work, but it might be slow
-- how does lucid work?

-- classes :: [Class] -> UI a
-- classes