-- Part 2 of the lab
-- Author: Johan Gustafsson

import Expr
import Data.Maybe
import Test.QuickCheck

prop_ShowReadExpr :: Expr -> Double -> Bool
prop_ShowReadExpr e x = doubleIsEqual (eval e x) (eval (fromJust (readExpr $ showExpr e )) x)

-- Consider floating point error, lazy implementation I know
doubleIsEqual :: Double -> Double -> Bool
doubleIsEqual d1 d2 = abs(d1-d2) < 0.01

-- This checks so that no given Expr will contain the forbidden
-- expression specified in the simplify function and lab pm.
-- No numbers should 
prop_SimplifyExpr :: Expr -> Bool
prop_SimplifyExpr e = exprIsValid (simplify e)
        where exprIsValid (Num x)         = True
              exprIsValid (Var)           = True
              exprIsValid (UnF f e)       = True && (exprIsValid e)
              exprIsValid (BiF Add e1 e2) = case (BiF Add e1 e2) of 
                  (BiF Add (Num x) (Num y))   -> False
                  (BiF Add e1 (Num 0))        -> False
                  (BiF Add (Num 0) e2)        -> False
                  (BiF Add e1 e2)             -> (exprIsValid e1) && (exprIsValid e2)
              exprIsValid (BiF Mul e1 e2) = case (BiF Mul e1 e2) of 
                  (BiF Mul (Num x) (Num y))   -> False
                  (BiF Mul (Num 0) _)         -> False
                  (BiF Mul _ (Num 0))         -> False
                  (BiF Mul (Num 1) e)         -> False
                  (BiF Mul e (Num 1))         -> False
                  (BiF Mul e1 e2)             -> (exprIsValid e1) && (exprIsValid e2)


-- Tells quickCheck how to generate test expressions.
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [ 
                      (1, do n <- arbitrary
                             return (Num (abs n)))
                    , (s, do a <- arbExpr s'
                             b <- arbExpr s'
                             return (BiF Add a b))
                    , (s, do a <- arbExpr s'
                             b <- arbExpr s'
                             return (BiF Mul a b))
                    , (s, do a <- arbExpr s'
                             return (UnF Sin a))
                    , (s, do a <- arbExpr s'
                             return (UnF Cos a))
                    , (s, do arbExpr s'
                             return Var)]
                where s' = s `div` 2

instance Arbitrary Expr where
  arbitrary = sized arbExpr