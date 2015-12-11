-- Part 1 of the lab
-- Author: Johan Gustafsson

import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck

-- Data type for representing simple mathematical expressions
data Expr
    = Num Double
    | Var Char
    | Add Expr Expr
    | Mul Expr Expr
    | Func FuncType
    deriving (Eq,Show)

-- Added a separate data type for function types
-- to enable easier/clearer extension.
data FuncType = Sin Expr | Cos Expr
    deriving (Eq, Show)


showExpr :: Expr -> String
showExpr (Num a)     = show a
showExpr (Var x)     = "x"
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showExpr e1 ++ " * " ++ showExpr e2
showExpr (Func f)    = showFunc f
        where showFunc (Sin e) = "sin(" ++ showExpr e ++ ")"
              showFunc (Cos e) = "cos(" ++ showExpr e ++ ")"


eval :: Expr -> Double -> Double
eval (Num n) _     = n
eval (Var x) v     = v
eval (Add e1 e2) v = (eval e1 v) + (eval e2 v)
eval (Mul e1 e2) v = (eval e1 v) * (eval e2 v)
eval (Func f) v    = applyFunc f v
        where applyFunc (Sin e) v = sin (eval e v)
              applyFunc (Cos e) v = cos (eval e v)


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