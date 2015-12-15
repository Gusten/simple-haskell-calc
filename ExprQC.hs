-- Part 2 of the lab
-- Author: Johan Gustafsson

prop_ShowReadExpr :: Expr -> Double -> Bool
prop_ShowReadExpr e x = doubleIsEqual (eval e x) (eval (fromJust (readExpr $ showExpr e )) x)

-- Consider floating point error, lazy implementation I know
doubleIsEqual :: Double -> Double -> Bool
doubleIsEqual d1 d2 = abs(d1-d2) < 0.0001


-- Tells quickCheck how to generate test expressions.
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [ 
                      (1, do n <- arbitrary
                             return (Num (abs n)))
                    , (s, do a <- arbExpr s'
                             b <- arbExpr s'
                             return (Add a b))
                    , (s, do a <- arbExpr s'
                             b <- arbExpr s'
                             return (Mul a b))
                    , (s, do a <- arbExpr s'
                             return (Sin a))
                    , (s, do a <- arbExpr s'
                             return (Cos a))
                    , (s, do arbExpr s'
                             return Var)]
                where s' = s `div` 2

instance Arbitrary Expr where
  arbitrary = sized arbExpr