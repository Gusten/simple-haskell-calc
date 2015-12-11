-- Part 1 of the lab
-- Author: Johan Gustafsson


instance Show Expr where
  show = showExpr


showExpr :: Expr -> String
showExpr = undefined


eval :: Expr -> Double -> Double
eval = undefined


readExpr :: String -> Maybe Expr
readExpr = undefined

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined


arbExpr :: Int -> Gen Expr
arbExpr = undefined

instance Arbitrary Expr where
  arbitrary = sized arbExpr


simplify :: Expr -> Expr
simplify = undefined


differentiate :: Expr -> Expr
differentiate = undefined