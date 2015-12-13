-- Part 1 of the lab
-- Author: Johan Gustafsson

import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck
import Data.List
import Control.Applicative

-- Data type for representing simple mathematical expressions
data Expr
    = Num Double
    | Var
    | Add Expr Expr
    | Mul Expr Expr
    | Sin Expr
    | Cos Expr
        deriving (Eq, Show)


showExpr :: Expr -> String
showExpr (Num a)     = show a
showExpr (Var)       = "x"
showExpr (Sin e)     = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e)     = "cos(" ++ showExpr e ++ ")"
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
        where showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
              showFactor e           = showExpr e


eval :: Expr -> Double -> Double
eval (Num n) _     = n
eval (Var) v       = v
eval (Sin e) v     = sin (eval e v)
eval (Cos e) v     = cos (eval e v)
eval (Add e1 e2) v = (eval e1 v) + (eval e2 v)
eval (Mul e1 e2) v = (eval e1 v) * (eval e2 v)


numVarParser :: Parser Expr
numVarParser  = fmap Num doubleParser +++ varParser

readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                     Just (e,"") -> Just e
                     _           -> Nothing

expr :: Parser Expr
expr = foldr1 Add `fmap` chain term (char '+')

term :: Parser Expr
term = foldr1 Mul `fmap` chain funcTerm (char '*')

funcTerm :: Parser Expr
funcTerm = (fmap Cos (cosParser >-> factor) 
       +++ fmap Sin (sinParser >-> factor))
       +++ factor

factor :: Parser Expr
factor = char '(' >-> expr <-< char ')' +++ numVarParser

sinParser :: Parser Char
sinParser = do 
    char 's'
    char 'i'
    char 'n'

cosParser :: Parser Char
cosParser = do 
    char 'c'
    char 'o'
    char 's'

varParser :: Parser Expr
varParser = do char 'x' 
               return Var


doubleParser :: Parser Double
doubleParser = do num <- readsP
                  return num


prop_ShowReadExpr :: Expr -> Double -> Bool
prop_ShowReadExpr e x = doubleIsEqual (eval e x) (eval (fromJust (readExpr $ showExpr e )) x)

-- Consider floating point error, lazy implementation I know
doubleIsEqual :: Double -> Double -> Bool
doubleIsEqual d1 d2 = abs(d1-d2) < 0.0001


-- Tells quickCheck how to generate test expressions.
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [ 
                      (1, do n <- arbitrary
                             if n > 0 then
                                return (Num n)
                             else
                                return (Num (n*(-1))))
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


-- I am not going to calculate sin()cos(). First of all it's not exact
-- and the approximation is gonna be waaay ugly.
-- Also I missed the part with no including variables. Well I already did it so...
simplify :: Expr -> Expr
simplify Var     = Var
simplify (Num x) = Num x
simplify (Sin e) = Sin (simplify e)
simplify (Cos e) = Cos (simplify e)
simplify (Add e1 e2) = case (Add p q) of 
                      (Add (Num x) (Num y))       -> Num $ x+y
                      (Add (Num 0) q) -> q
                      (Add p (Num 0)) -> p
                      (Add (Mul (Num x) a) b) | a == b -> (Mul (Num $ x+1) a)
                      (Add a (Mul (Num x) b)) | a == b -> (Mul (Num $ x+1) a)
                      (Add (Mul a (Num x)) b) | a == b -> (Mul (Num $ x+1) a)
                      (Add a (Mul b (Num x))) | a == b -> (Mul (Num $ x+1) a)        
                      (Add (Mul (Num x) a) (Mul (Num y) b)) | a == b -> (Mul (Num $ x+y) a)
                      (Add (Mul (Num x) a) (Mul b (Num y))) | a == b -> (Mul (Num $ x+y) a)
                      (Add (Mul a (Num x)) (Mul b (Num y))) | a == b -> (Mul (Num $ x+y) a)
                      (Add (Mul a (Num x)) (Mul (Num y) b)) | a == b -> (Mul (Num $ x+y) a)
                      (Add x y)                   -> (Add x y)
            where p = simplify e1
                  q = simplify e2
simplify (Mul e1 e2) = case (Mul p q) of 
                      (Mul (Num x) (Num y)) -> Num $ x*y
                      (Mul (Num 0) _)       -> Num 0
                      (Mul _ (Num 0))       -> Num 0
                      (Mul (Num 1) q)       -> q
                      (Mul p (Num 1))       -> p       
                      (Mul (Mul (Num x) a) (Mul (Num y) b)) | a == b -> (Mul (Num $ x*y) (Mul a b))
                      (Mul (Mul (Num x) a) (Mul b (Num y))) | a == b -> (Mul (Num $ x*y) (Mul a b))
                      (Mul (Mul a (Num x)) (Mul b (Num y))) | a == b -> (Mul (Num $ x*y) (Mul a b))
                      (Mul (Mul a (Num x)) (Mul (Num y) b)) | a == b -> (Mul (Num $ x*y) (Mul a b))
                      (Mul x y)             -> (Mul x y)
            where p = simplify e1
                  q = simplify e2


-- Differentiate the given expression with respect to x(Var)
-- Using simplify to keep things nice and tidy after differentation.
differentiate :: Expr -> Expr
differentiate Var               = Num 1
differentiate (Num _)           = Num 0
differentiate (Add e1 e2)       = Add (differentiate e1) (differentiate e2)
differentiate (Mul (Num x) Var) = Num x
differentiate (Mul Var (Num x)) = Num x
differentiate (Mul e (Sin ie))  = Mul (newSimpleS e ie) (Cos ie)
differentiate (Mul (Sin ie) e)  = Mul (newSimpleS e ie) (Cos ie)
differentiate (Mul e (Cos ie))  = Mul (newSimpleS e ie) (Sin ie)
differentiate (Mul (Cos ie) e)  = Mul (newSimpleS e ie) (Sin ie)

newSimpleS e ie = simplify (Mul e (differentiate ie))
newSimpleC e ie = simplify (Mul (Mul (Num (-1.0)) e) (differentiate ie))