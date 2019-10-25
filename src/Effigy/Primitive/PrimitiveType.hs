module Effigy.Primitive.PrimitiveType where

data PrimitiveType
  = UnitType
  | BooleanType
  | NaturalType

instance Show PrimitiveType where
  show = \case
    UnitType    -> "Unit"
    BooleanType -> "Boolean"
    NaturalType -> "Natural"
