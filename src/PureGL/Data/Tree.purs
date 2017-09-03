module PureGL.Data.Tree where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.List (List(..), snoc, (:))
import Data.Monoid (power)
import Data.Traversable (class Traversable, sequenceDefault, traverse)

-- | A Rose, or multi-way tree, with values of type `a`.
newtype Tree a = Node { value :: a 
                      , children :: Forest a
                      }

-- | A colletcion of `Tree`s.
type Forest a = List (Tree a)

-- | Create a `Tree` from a value and a `Forest`.
mkTree :: forall a. a -> Forest a -> Tree a
mkTree v c = Node { value: v, children: c }

infix 5 mkTree as |> 

-- | Get the value of a `Tree` `Node`.
head :: forall a. Tree a -> a
head (Node {value: a, children: _}) = a

-- | Get the `Forest` representing the children of a `Tree` `Node`.
tail :: forall a. Tree a -> Forest a
tail (Node {value: _, children: c}) = c

instance functorTree :: Functor Tree where
  map f (Node {value:v, children: c}) = 
    mkTree (f v) ((map <<< map) f c)

instance applyTree :: Apply Tree where
  apply (Node {value: f, children: fs}) tx@(Node {value: x, children: xs})
    = Node {value: (f x), children: ((map <<< map) f xs) <> (map ((flip apply) tx) fs)}

instance applicativeTree :: Applicative Tree where
  pure = (flip $ mkTree) Nil

instance bindTree :: Bind Tree where
  bind tx@(Node {value: x, children: xs}) f = 
    let (Node {value: b, children: bs}) = f x in
    Node {value: b, children: bs <> (map ((flip bind) f) xs)}

instance monadTree :: Monad Tree

instance foldableTree :: Foldable Tree where
  foldMap f (Node {value: x, children: xs}) = (f x) <> (foldMap (foldMap f) xs)
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance traversableTree :: Traversable Tree where
  traverse f (Node {value: x, children: xs}) =
    lift2 (\a b -> Node {value: a, children: b}) (f x) (traverse (traverse f) xs)

  sequence x = sequenceDefault x  

-- | Draw the 2D `String` representation of a `Tree String`.
drawTree :: Tree String -> String
drawTree t = tailRec go {level: 0, drawn: (head t) <> "\n", current: (tail t)}
  where
    go :: _ -> Step _ String
    go {level: l, drawn: s, current: Nil} = Done s
    go {level: l, drawn: s, current: c:cs } = 
      let drawn = (power "       " l) <> "|----> " <> (head c) <> "\n" in
      Loop {level: l, drawn: s <> drawn <> (tailRec go {level: l + 1, drawn: "", current: (tail c)})  , current: cs}

-- | Draw the 2D `String`  representation of a `Tree` composed of `Show`able
-- | elements.
showTree :: forall a. Show a => Tree a -> String
showTree = drawTree <<< (map show)

-- | Scan a `Tree`, accumulating values of `b` there are constant across `Node`s
-- | of the same level.
scanTree :: forall a b. (a -> b -> b) -> b -> Tree a -> Tree b
scanTree f b n@(Node {value: x, children: xs}) = 
  let fb = f x b 
  in fb |> (tailRec go {b: fb, current: xs, final: Nil})

  where

    go :: _ -> Step _ (Forest b)
    go {b: b', current: Nil, final: final} = Done final
    go {b: b', current: c:cs, final: final} = 
      let fb' = f (head c) b' 
      in Loop {b: b', current: cs, final: snoc final (fb' |> tailRec go {b: fb', current: (tail c), final: Nil})}
