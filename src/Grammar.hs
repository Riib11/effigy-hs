{-# LANGUAGE LambdaCase #-}

module Grammar where

{-
  # Grammar
-}

data Program
  = Library    [Declaration]
  | Executable [Declaration] Expression
  deriving (Show)

data Declaration
  = Effect Name Type
  | Term  Name Type Expression
  deriving (Show)

data Expression
  = Name        Name
  | Value       Value
  | Application Expression Expression
  | Performance EffectName Expression
  | Binding     Name Expression Expression
  | Sequence    Sequence
  | DoWith      Expression Expression
  deriving (Show)

data Sequence
  = Sequenced Expression Sequence
  | Capture   Name Expression Sequence -- effectful binding
  | Terminal  Expression               -- return
  deriving (Show)

data Value
  = PrimitiveValue PrimitiveValue
  | Lambda         Name Expression
  | HandlerValue   Handler
  deriving (Show)

data Handler = Handler
  { handleEffect :: (Name, Name, Name, Expression)
  , handleValue  :: (Name, Expression)
  , handleRun    :: (Name, Expression) }
  deriving (Show)

data Type
  = PrimitiveType PrimitiveType
  | Function      Type Type
  | HandlerType   EffectNames Type
  | EffectType    EffectNames Type
  deriving (Show)

data PrimitiveValue = BooleanValue Bool deriving (Show)

data PrimitiveType = BooleanType deriving (Show)

type EffectName = Name

type EffectNames = [EffectName]

type Name = String

{-
  ## Utilities
-}

toValue :: Expression -> Maybe Value
toValue = \case
  Value v -> Just v
  _ -> Nothing

lambda n e = Value $ Lambda n e
handler h = Value $ HandlerValue h
-- handler hE hV hR = HandlerValue $ Handler hE hV hR
