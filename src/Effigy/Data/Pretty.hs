module Effigy.Data.Pretty where


import           Effigy.Data.Tree


{-
  # Pretty class

  A pretty can be represented as a pretty tree, which is displayed
-}

class Pretty (p :: * -> *) a where
  prettyTree :: p a -> Tree String
  pretty :: Show a => p a -> String
  pretty = show . prettyTree
  {-# MINIMAL prettyTree #-}

instance Show a => Pretty Tree a where
  prettyTree = fmap show

-- instance (Show b, Treelike t a b) => Pretty t a where
--   pretty = error "pretty"
