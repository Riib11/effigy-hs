module Effigy.GrammarPatterns where


import Effigy.Grammar

-- Expression
pattern e :●: e' = Application e e'
pattern ValueVariable v = Value (Variable v)
pattern ValuePrimitive pv = Value (PrimitiveValue pv)
pattern ValueSequence s = Value (Sequence s)
pattern ValueHandler hE hV hR = Value (HandlerValue (Handler hE hV hR))
pattern x :=>: e = Value (Lambda x e)
pattern ϕ :⧳: e = Value (Performance ϕ e)
-- Sequence
pattern e :>>: s = Sequenced e s
-- Type
pattern t :->: t' = Function t t'
pattern 𝐇 ϕs t = HandlerType ϕs t
pattern 𝐄 ϕs t = EffectType ϕs t
