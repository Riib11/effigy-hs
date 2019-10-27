import           Effigy.Grammar
import           Effigy.GrammarPatterns
import           Effigy.Rewriting
import           Data.Map                                          as Map
                                                             hiding ( map )
import           Effigy.Data.Tree
import           Effigy.Data.Pretty


{-
  # Main
-}

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ replicate 50 '='
  printReduceN 5 (expression_examples ! "single effect with abstract handler")
  putStrLn $ replicate 50 '='

printReduceN :: Int -> Expression -> IO ()
printReduceN n = printReductions . reduceN n


printReductions :: [Expression] -> IO ()
printReductions = \case
  [e] -> prettyPrintExpression e
  es  -> do
    mconcat <$> mapM (\e -> putStr $ prettyShowExpression e ++ "\n\n↠\n\n") (init es)
    prettyPrintExpression (last es)

reduceN :: Int -> Expression -> [Expression]
reduceN 0 e = [e]
reduceN n e = e : reduceN (pred n) (reduce e)


{-
  ## Example Expressions
-}

expression_examples = fromList
  [ ("function application"               , ("x" :=>: ("x" :#:)) :●: ("y" :#:))
  , ("single effect with abstract handler", ("p" :⧳: ("a" :#:)) `DoWith` handlerAbstract)
  ]

-- abstract handler
handlerAbstract = Value $ HandlerValue $ Handler hE hV hR where
  hE = ("p", "x", "k", ("e_p" :#:))
  hV = ("x", "k", ("e_v" :#:))
  hR = ("g", ("e_r" :#:))

-- -- sequenced effect
-- expr5 = Sequence (e1 `Sequenced` Terminal e2) `DoWith` handlerA where
--   e1 = Performance "A" $ Value $ Name "a"
--   e2 = Value $ Name "b"
--
-- -- single value
-- expr3 = e `DoWith` handlerA where e = Value $ PrimitiveValue UnitValue
--
-- -- sequenced value
-- expr4 = Sequence (e1 `Sequenced` (e2 `Sequenced` Terminal e3)) `DoWith` handlerA where
--   e1 = Value $ Name "a"
--   e2 = Value $ Name "b"
--   e3 = Value $ Name "c"
