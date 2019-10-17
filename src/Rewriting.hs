module Rewriting where

import           Grammar


{-
  # Rewriting
-}


-- reduce via the first rewrite rule that applies, if there is one
rewrite :: Expression -> Expression
rewrite eOld = case eOld of
  --
  -- functional foundations
  --
  Name x -> eOld
  Value v -> eOld
  Application (Value (Lambda x e1)) e2 -> substitute e2 x e1
  Binding x e1 e2 -> Application (lambda x e2) e1
  --
  -- sequences of effects
  --
  Sequence (Terminal e) -> e
  --
  Sequence (Capture x e s) `DoWith` h -> Binding x (e `DoWith` h) (Sequence s `DoWith` h)
  --
  -- handle effect
  --
  Sequence (Performance p e `Sequenced` s) `DoWith` Value (HandlerValue h) ->
    let (p', x, k, ep) = handleEffect h in
    if p /= p' then eOld else -- check that h can handle this effect
      ((lambda k (lambda x ep)) `Application` e) `Application` (Sequence s `DoWith` handler h)
  --
  -- handle run
  --
  Sequence (Terminal e) `DoWith` Value (HandlerValue h) ->
    let (g, er) = handleRun h in
    (lambda g er) `Application` e
  --
  -- handle value
  --
  Sequence (e `Sequenced` s) `DoWith` Value (HandlerValue h) ->
    let (x, ev) = handleValue h in
    (lambda x ev `Application` e) `Application` (Sequence s `DoWith` handler h)


{-
  ## Noramlization
-}


normalize :: Expression -> Value
normalize = error "TODO"


{-
  # Substitutions
-}


substitute :: Expression -> Name -> Expression -> Expression
substitute eNew xOld eOld =
  let r = substitute eNew xOld in
  case eOld of
    Name        x                -> if x == xOld then eNew else eOld
    Value (PrimitiveValue pv)    -> eOld
    Value       (Lambda x e)     -> if x == xOld then eOld else Value $ Lambda x (r e)
    Value       (HandlerValue h) -> Value $ HandlerValue $ substituteHandler eNew xOld h
    Application e1 e2            -> Application (r e1) (r e2)
    Binding     x e1 e2          -> Binding x (r e1) (if x == xOld then e2 else r e2)
    Sequence    s                -> Sequence $ substituteSequence eNew xOld s
    DoWith      e1 e2            -> DoWith (r e1) (r e2)

substituteSequence :: Expression -> Name -> Sequence -> Sequence
substituteSequence eNew xOld sOld =
  let r  = substituteSequence eNew xOld
      re = substitute eNew xOld in
  case sOld of
    e `Sequenced` s -> re e `Sequenced` r s
    Capture x e s   -> if x == xOld then sOld else Capture x (re e) (r s)
    Terminal e      -> Terminal (re e)

substituteHandler :: Expression -> Name -> Handler -> Handler
substituteHandler eNew xOld h =
  let r = substitute eNew xOld
      hE = handleEffect h
      hV = handleValue h
      hR = handleRun h in
  Handler (let (p, x, k, e) = hE in if xOld `elem` [p, x, k] then hE else (p, x, k, r e))
          (let (x, e) = hV in if xOld == x then hV else (x, r e))
          (let (g, e) = hR in if xOld == g then hR else (g, r e))
