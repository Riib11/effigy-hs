module Effigy.Primitive.PrimitiveValue where

import           Effigy.Primitive.Natural

data PrimitiveValue
  = UnitValue
  | BooleanValue Bool
  | NaturalValue ℕ

instance Show PrimitiveValue where
  show = \case
    UnitValue      -> "()"
    BooleanValue b -> if b then "true" else "false"
    NaturalValue n -> show n
