module Effigy.Data.Tree where

data Tree a = Leaf a | Branch a [Tree a]

instance Monoid a => Semigroup (Tree a) where
  t <> t' = Branch mempty [t, t']

instance Show a => Show (Tree a) where
  show = recurse 0 . fmap show   where
    recurse :: Int -> Tree String -> String
    recurse i = \case
      Leaf s       -> indent i ++ s ++ "\n"
      Branch s pts -> (indent i ++ s ++ "\n") ++ (concat $ map (recurse $ succ i) pts)
    indent i = concat . replicate i $ "  "

showStringTree :: Tree String -> String
showStringTree = recurse 0 where
  recurse :: Int -> Tree String -> String
  recurse i = \case
    Leaf s        -> indent i ++ s ++ "\n"
    Branch "" pts -> concat $ map (recurse i) pts
    Branch s  pts -> (indent i ++ s ++ "\n") ++ (concat $ map (recurse $ succ i) pts)
  indent i = concat . replicate i $ "  "

instance Functor Tree where
  fmap f = \case
    Leaf a      -> Leaf (f a)
    Branch a ts -> Branch (f a) (map (fmap f) ts)

data TransTree a b = TransLeaf (a,b) | TransBranch (a,b) [TransTree a b]

class Treelike (t :: * -> *) a b where
  treeOf :: t a -> TransTree a b
