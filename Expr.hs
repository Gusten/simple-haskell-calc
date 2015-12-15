-- Part 1 of the lab
-- Author: Johan Gustafsson

module Expr where

import Parsing
import Data.Char
import Data.Maybe
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


-- I am not going to calculate sin()cos(). First of all it's not exact
-- and the approximation is gonna be waaay ugly.
-- Also I missed the part with no including variables. Well I already did it so...
-- This quickly turned into quite a monster didn't it
simplify :: Expr -> Expr
simplify Var     = Var
simplify (Num x) = Num x
simplify (Sin e) = Sin (simplify e)
simplify (Cos e) = Cos (simplify e)
simplify (Add e1 e2) = case (Add p q) of 
      (Add (Num x) (Num y))                          -> Num $ x+y
      (Add (Num 0) q)                                -> simplify q
      (Add p (Num 0))                                -> simplify p
      (Add (Mul (Num x) a) b) | a == b               -> (Mul (Num $ x+1) (simplify a))
      (Add a (Mul (Num x) b)) | a == b               -> (Mul (Num $ x+1) (simplify a))
      (Add (Mul a (Num x)) b) | a == b               -> (Mul (Num $ x+1) (simplify a))
      (Add a (Mul b (Num x))) | a == b               -> (Mul (Num $ x+1) (simplify a))
      (Add (Mul (Num x) a) (Mul (Num y) b)) | a == b -> (Mul (Num $ x+y) (simplify a))
      (Add (Mul (Num x) a) (Mul b (Num y))) | a == b -> (Mul (Num $ x+y) (simplify a))
      (Add (Mul a (Num x)) (Mul b (Num y))) | a == b -> (Mul (Num $ x+y) (simplify a))
      (Add (Mul a (Num x)) (Mul (Num y) b)) | a == b -> (Mul (Num $ x+y) (simplify a))
      (Add x y)                                      -> (Add x y)
    where p = simplify e1
          q = simplify e2
simplify (Mul e1 e2) = case (Mul p q) of 
      (Mul (Num x) (Num y))                          -> Num $ x*y
      (Mul (Num 0) _)                                -> Num 0
      (Mul _ (Num 0))                                -> Num 0
      (Mul (Num 1) q)                                -> simplify q
      (Mul p (Num 1))                                -> simplify p 
      (Mul (Num x) (Mul (Num y) e))                  -> Mul (Num (x*y)) $ simplify e
      (Mul (Num x) (Mul e (Num y)))                  -> Mul (Num (x*y)) $ simplify e
      (Mul (Mul (Num x) e) (Num y))                  -> Mul (Num (x*y)) $ simplify e
      (Mul (Mul e (Num x)) (Num y))                  -> Mul (Num (x*y)) $ simplify e
      (Mul (Mul (Num x) a) (Mul (Num y) b)) | a == b -> (Mul (Num $ x*y) (Mul n n))
                                where n = simplify a
      (Mul (Mul (Num x) a) (Mul b (Num y))) | a == b -> (Mul (Num $ x*y) (Mul n n))
                                where n = simplify a
      (Mul (Mul a (Num x)) (Mul b (Num y))) | a == b -> (Mul (Num $ x*y) (Mul n n))
                                where n = simplify a
      (Mul (Mul a (Num x)) (Mul (Num y) b)) | a == b -> (Mul (Num $ x*y) (Mul n n))
                                where n = simplify a
      (Mul x y)                                      -> (Mul x y)
    where p = simplify e1
          q = simplify e2


-- Differentiate the given expression with respect to x(Var)
-- Using simplify to keep things nice and tidy after differentation.
differentiate :: Expr -> Expr
differentiate Var               = Num 1
differentiate (Num _)           = Num 0
differentiate (Sin e)           = simplify $ Mul (differentiate e) (Cos e)
differentiate (Cos e)           = simplify $ Mul (Num (-1)) (Mul (differentiate e) (Sin e))
differentiate (Add e1 e2)       = simplify $ Add (differentiate e1) (differentiate e2)
differentiate (Mul e1 e2)       = simplify $ Add (Mul (differentiate e1) e2) (Mul e1 (differentiate e2))

-- Just a shortcut function so I can write expressions as Strings and try them out
readAndDiff :: String -> Expr
readAndDiff s = differentiate $ fromJust $ readExpr s