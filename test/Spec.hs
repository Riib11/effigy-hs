import           Grammar
import           Rewriting

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ replicate 50 '='
  print expr
  putStrLn "\n->>\n"
  print $ rewrite expr
  putStrLn $ replicate 50 '='

{-
  ## Example Expressions
-}

expr = expr4

expr1 = (Value $ Lambda "x" $ Value $ Name "x") `Application` (Value $ Name "x")

handlerA = Value $ HandlerValue $ Handler hE hV hR where
  hE = ("A", "x", "k", Value $ Name "e_p")
  hV = ("x", Value $ Name "e_v")
  hR = ("g", Value $ Name "e_r")

-- single effect
expr2 = e `DoWith` handlerA where
  e = Performance "A" $ Value $ Name "a"

-- sequenced effect
expr5 = Sequence (e1 `Sequenced` Terminal e2) `DoWith` handlerA where
  e1 = Performance "A" $ Value $ Name "a"
  e2 = Value $ Name "b"

-- single value
expr3 = e `DoWith` handlerA where
  e = Value $ PrimitiveValue UnitValue

-- sequenced value
expr4 = Sequence (e1 `Sequenced` (e2 `Sequenced` Terminal e3)) `DoWith` handlerA where
  e1 = Value $ Name "a"
  e2 = Value $ Name "b"
  e3 = Value $ Name "c"
