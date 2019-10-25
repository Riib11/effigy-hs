module Effigy.Grammar where

import           Effigy.Primitive.PrimitiveType
import           Effigy.Primitive.PrimitiveValue

{-
  # Grammar
-}

data Program
  = Module     [Declaration]
  | Executable [Declaration] Expression

data Declaration
  = Effect       Name Type Type
  | Construction Name Type Expression

data Expression
  = Value       Value
  | Application Expression Expression
  | Binding     Name Expression Expression
  | DoWith      Expression Expression

data Value
  = Variable       TermName
  | PrimitiveValue PrimitiveValue
  | Lambda         Name Expression
  | Performance    EffectName Expression
  | Sequence       Sequence
  | HandlerValue   Handler

data Sequence
  = Sequenced Expression Sequence
  | Capture   Name Expression Sequence -- effectful binding
  | Terminal  Expression               -- return

data Handler = Handler
  { handleEffect :: (Name, Name, Name, Expression)
  , handleValue  :: (Name, Name, Expression)
  , handleRun    :: (Name, Expression) }

data Type
  = PrimitiveType PrimitiveType
  | Function      Type Type
  | HandlerType   EffectNames Type
  | EffectType    EffectNames Type

type TermName = Name
type EffectName = Name
type EffectNames = [EffectName]
type Name = String


{-
  ## Show Instances
-}

instance Show Program where
  show = \case
    Module ds       -> unlines $ map show ds
    Executable ds e -> unlines $ map show ds ++ [unwords ["Main", ":=", show e, "."]]

instance Show Declaration where
  show = \case
    Effect       ϕ s t -> unwords ["Effect", ϕ, ":", show s, "~>", show t, "."]
    Construction x t e -> unwords ["Construction", x, ":", show t, ":=", show e, "."]

instance Show Expression where
  show = \case
    Value v           -> show v
    Application e1 e2 -> unwords [show e1, show e2]
    Binding n e1 e2   -> unwords [n, ":=", show e1, ";", show e2]
    DoWith e h        -> unwords ["(", "do", show e, "with", show h, ")"]

instance Show Sequence where
  show = \case
    Sequenced e1 e2 -> unwords [show e1, ";", show e2]
    Capture x e1 e2 -> unwords [x, "<-", show e1, ";", show e2]
    Terminal e      -> show e

instance Show Value where
  show = \case
    Variable       x  -> x
    PrimitiveValue pv -> show pv
    Lambda      x e   -> unwords ["(", x, "=>", show e, ")"]
    Performance ϕ e   -> unwords [ϕ, show e]
    HandlerValue h    -> show h

instance Show Handler where
  show (Handler hE hV hR) = unwords
    [ "handler"
    , "{"
    , let (ϕ, x, k, eϕ) = hE in unwords ["effect", "(", ϕ, x, ")", k, "=>", show eϕ]
    , "|"
    , let (x, k, ev) = hV in unwords ["value", x, k, "=>", show ev]
    , "|"
    , let (g, er) = hR in unwords ["run", g, "=>", show er]
    , "}"
    ]

instance Show Type where
  show = \case
    PrimitiveType pt -> show pt
    Function    s  t -> unwords [show s, "->", show t]
    HandlerType ϕs t -> unwords ["H", show ϕs, "(", show t, ")"]
    EffectType  ϕs t -> unwords ["E", show ϕs, "(", show t, ")"]

{-
  ## Utilities
-}

toValue :: Expression -> Maybe Value
toValue = \case
  Value v -> Just v
  _       -> Nothing

lambda n e = Value $ Lambda n e
handler h = Value $ HandlerValue h
