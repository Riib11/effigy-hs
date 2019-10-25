module Effigy.Data.Pretty where


{-
  # Pretty class

  A pretty can be represented as a pretty tree, which is displayed
-}

class Show a => Pretty a where
  pretty :: a -> PrettyTree a
  showPretty :: a -> String
  showPretty = show . pretty
  {-# MINIMAL pretty #-}


data PrettyTree a = PrettyNode a | PrettyBranch a [PrettyTree a]

instance Show a => Show (PrettyTree a) where
  show = concat . recurse 0
   where
    recurse i = \case
      PrettyNode a       -> [indent i ++ show a]
      PrettyBranch a pts -> let s = show a in (indent i ++ s) : (concat $ map (recurse $ i + length s) pts)

indent i = replicate i ' '
