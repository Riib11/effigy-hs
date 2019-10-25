module Effigy.Rewriting where

import           Effigy.Grammar


{-
  # Rewriting
-}


{-
  ## Reduction
-}


-- reduce via the first rewrite rule that applies, if there is one
reduce :: Expression -> Expression
reduce eOld = case eOld of
  --
  -- sequences of effects
  --
  -- terminal
  Value (Sequence (Terminal e))               -> e
  -- capture
  Value (Sequence (Capture x e s)) `DoWith` h -> Binding x e (Value (Sequence s) `DoWith` h)

  --
  -- handle effect
  --
  -- unsequenced
  Value (Performance ϕ e) `DoWith` Value (HandlerValue (Handler hE hV hR)) ->
    let (ϕ', x, k, eϕ) = hE
        (g, er)        = hR
    in  if ϕ /= ϕ'
          then eOld
          else -- if h can handle this effect
               (lambda k ((lambda x eϕ) `Application` e)) `Application` (lambda g er)
  -- sequenced
  Value (Sequence (Value (Performance ϕ e) `Sequenced` s)) `DoWith` h@(Value (HandlerValue (Handler hE hV hR))) ->
    let (ϕ', x, k, eϕ) = hE
    in  if ϕ /= ϕ'
          then eOld
          else -- if h can handle this effect
               (lambda k ((lambda x eϕ) `Application` e)) `Application` (Value (Sequence s) `DoWith` h)

  --
  -- handle run
  --
  -- singleton sequence
  Value (Sequence (Terminal e)) `DoWith` Value (HandlerValue (Handler hE hV hR)) ->
    let (g, er) = hR in (lambda g er) `Application` e

  --
  -- handle value
  --
  -- sequenced
  Value (Sequence (e `Sequenced` s)) `DoWith` h@(Value (HandlerValue (Handler hE hV hR))) ->
    let (x, k, ev) = hV in lambda k (lambda x ev `Application` e) `Application` (Value (Sequence s) `DoWith` h)
  -- unsequenced
  e `DoWith` Value (HandlerValue (Handler hE hV hR)) ->
    let (x, k, ev) = hV
        (g, er)    = hR
    in  lambda k (lambda x ev `Application` e) `Application` lambda g er

  --
  -- functional foundations
  --
  Value v                              -> Value v
  Application (Value (Lambda x e1)) e2 -> substitute e2 x e1
  Binding x e1 e2                      -> Application (lambda x e2) e1


{-
  ## Noramlization
-}


normalize :: Expression -> Expression
normalize =
  let rec = normalize
  in  \case
        Value v           -> Value $ simplify v
        Application e1 e2 -> reduce $ rec e1 `Application` rec e2
        Binding x e1 e2   -> reduce $ Binding x (rec e1) (rec e2)
        DoWith e1 e2      -> reduce $ rec e1 `DoWith` rec e2

normalizeSequence :: Sequence -> Sequence
normalizeSequence = \case
  Sequenced e1 s -> Sequenced (normalize e1) (normalizeSequence s)
  Capture x e s  -> Capture x (normalize e) (normalizeSequence s)
  Terminal e     -> Terminal (normalize e)


{-
  ## Simplification
-}

simplify :: Value -> Value
simplify = \case
  Variable       x  -> Variable x
  PrimitiveValue pv -> PrimitiveValue pv
  Lambda x e        -> Lambda x $ normalize e
  HandlerValue h    -> HandlerValue $ simplifyHandler h

simplifyHandler :: Handler -> Handler
simplifyHandler = undefined

{-
  ## Substitution
-}


substitute :: Expression -> Name -> Expression -> Expression
substitute eNew xOld eOld =
  let rec    = substitute eNew xOld
      recSeq = substituteSequence eNew xOld
      recHan = substituteHandler eNew xOld
  in  case eOld of
        Value (Variable       x ) -> if x == xOld then eNew else eOld
        Value (PrimitiveValue pv) -> eOld
        Value (Lambda x e       ) -> if x == xOld then eOld else Value . Lambda x $ rec e
        Value (Sequence s       ) -> Value . Sequence $ recSeq s
        Value (Performance ϕ e  ) -> Value . Performance ϕ $ rec e
        Value (HandlerValue h   ) -> Value . HandlerValue $ recHan h
        Application e1 e2         -> Application (rec e1) (rec e2)
        Binding x e1 e2           -> Binding x (rec e1) (if x == xOld then e2 else rec e2)
        DoWith e1 e2              -> DoWith (rec e1) (rec e2)

substituteSequence :: Expression -> Name -> Sequence -> Sequence
substituteSequence eNew xOld sOld =
  let rec    = substituteSequence eNew xOld
      recExp = substitute eNew xOld
  in  case sOld of
        Sequenced e s -> recExp e `Sequenced` rec s
        Capture x e s -> if x == xOld then sOld else Capture x (recExp e) (rec s)
        Terminal e    -> Terminal $ recExp e

substituteHandler :: Expression -> Name -> Handler -> Handler
substituteHandler eNew xOld (Handler hE hV hR) =
  let rec = substitute eNew xOld
  in  Handler (let (p, x, k, e) = hE in if xOld `elem` [p, x, k] then hE else (p, x, k, rec e))
              (let (x, k, e) = hV in if xOld `elem` [x, k] then hV else (x, k, rec e))
              (let (g, e) = hR in if xOld `elem` [g] then hR else (g, rec e))
