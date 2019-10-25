module Effigy.Rewriting where

import           Effigy.Grammar
import           Effigy.GrammarPatterns


{-
  # Rewriting

  Rewrite rules define how an expression reduces.
  A computable expression will reduce to a value.
  Every expression reduces to a normal form.
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
  ValueSequence (Terminal e)               -> e
  -- capture
  ValueSequence (Capture x e s) `DoWith` h -> Binding x e (ValueSequence s `DoWith` h)

  --
  -- handle effect
  --
  -- unsequenced
  ϕ :⧳: e `DoWith` ValueHandler hE hV hR ->
    let (ϕ', x, k, eϕ) = hE
        (g, er)        = hR
    in  if ϕ /= ϕ'
          then eOld
          else -- if h can handle this effect
               (k :=>: ((x :=>: eϕ) :●: e)) :●: (g :=>: er)
  -- sequenced
  ValueSequence (ϕ :⧳: e :>>: s) `DoWith` h@(ValueHandler hE hV hR) ->
    let (ϕ', x, k, eϕ) = hE
    in  if ϕ /= ϕ'
          then eOld
          else -- if h can handle this effect
               (k :=>: ((x :=>: eϕ) :●: e)) :●: (ValueSequence s `DoWith` h)

  --
  -- handle run
  --
  -- singleton sequence
  ValueSequence (Terminal e) `DoWith` ValueHandler hE hV hR -> let (g, er) = hR in (g :=>: er) :●: e

  --
  -- handle value
  --
  -- sequenced
  ValueSequence (e :>>: s) `DoWith` h@(ValueHandler hE hV hR) ->
    let (x, k, ev) = hV in lambda k (lambda x ev :●: e) :●: (ValueSequence s `DoWith` h)
  -- unsequenced
  e `DoWith` ValueHandler hE hV hR ->
    let (x, k, ev) = hV
        (g, er)    = hR
    in  lambda k (lambda x ev :●: e) :●: lambda g er

  --
  -- functional foundations
  --
  Value v                              -> Value v
  Application (Value (Lambda x e1)) e2 -> substitute e2 x e1
  Binding x e1 e2                      -> (x :=>: e2) :●: e1


{-
  ## Noramlization

  Every expression reduces to a normal form.
-}


normalize :: Expression -> Expression
normalize =
  let rec = normalize
  in  \case
        Value v           -> Value $ simplify v
        Application e1 e2 -> reduce $ rec e1 :●: rec e2
        Binding x e1 e2   -> reduce $ Binding x (rec e1) (rec e2)
        DoWith e1 e2      -> reduce $ rec e1 `DoWith` rec e2

normalizeSequence :: Sequence -> Sequence
normalizeSequence = \case
  Sequenced e1 s -> Sequenced (normalize e1) (normalizeSequence s)
  Capture x e s  -> Capture x (normalize e) (normalizeSequence s)
  Terminal e     -> Terminal (normalize e)



{-
  ## Simplification

  Values cano be normalized like expressions, through simplification.
-}


simplify :: Value -> Value
simplify = \case
  Variable       x  -> Variable x
  PrimitiveValue pv -> PrimitiveValue pv
  Lambda x e        -> Lambda x $ normalize e
  HandlerValue h    -> HandlerValue $ simplifyHandler h

simplifyHandler :: Handler -> Handler
simplifyHandler (Handler hE hV hR) = Handler (let (p, x, k, e) = hE in (p, x, k, normalize e))
                                             (let (x, k, e) = hV in (x, k, normalize e))
                                             (let (g, e) = hR in (g, normalize e))

{-
  ## Substitution
-}


substitute :: Expression -> Name -> Expression -> Expression
substitute eNew xOld eOld =
  let rec    = substitute eNew xOld
      recSeq = substituteSequence eNew xOld
      recHan = substituteHandler eNew xOld
  in  case eOld of
        ValueVariable  x       -> if x == xOld then eNew else eOld
        ValuePrimitive pv      -> eOld
        x :=>: e               -> if x == xOld then eOld else x :=>: rec e
        ValueSequence s        -> ValueSequence $ recSeq s
        ϕ :⧳: e                -> ϕ :⧳: rec e
        Value (HandlerValue h) -> Value . HandlerValue $ recHan h
        e1 :●: e2              -> rec e1 :●: rec e2
        Binding x e1 e2        -> Binding x (rec e1) (if x == xOld then e2 else rec e2)
        e1 `DoWith` e2         -> rec e1 `DoWith` rec e2

substituteSequence :: Expression -> Name -> Sequence -> Sequence
substituteSequence eNew xOld sOld =
  let rec    = substituteSequence eNew xOld
      recExp = substitute eNew xOld
  in  case sOld of
        e :>>: s      -> recExp e :>>: rec s
        Capture x e s -> if x == xOld then sOld else Capture x (recExp e) (rec s)
        Terminal e    -> Terminal $ recExp e

substituteHandler :: Expression -> Name -> Handler -> Handler
substituteHandler eNew xOld (Handler hE hV hR) =
  let rec = substitute eNew xOld
  in  Handler (let (p, x, k, e) = hE in if xOld `elem` [p, x, k] then hE else (p, x, k, rec e))
              (let (x, k, e) = hV in if xOld `elem` [x, k] then hV else (x, k, rec e))
              (let (g, e) = hR in if xOld `elem` [g] then hR else (g, rec e))
