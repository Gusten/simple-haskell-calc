-- Part 1 of the lab
-- Author: Johan Gustafsson

-- 8th of january, the recap lecture. 2 lectures

module Expr where

import Parsing
import Data.Char
import Data.Maybe
import Data.List
import Control.Applicative

-- Data type for representing simple mathematical expressions

-- New function datatypes
-- Binary, uni
data Expr
    = Num Double
    | Var
    | UnF UFunc Expr
    | BiF BFunc Expr Expr
        deriving (Show)

class UOp a where
    udop :: a -> (Double -> Double)

data UFunc = Sin | Cos | Tan
    deriving (Show, Enum)

instance UOp UFunc where
    udop Sin = sin
    udop Cos = cos
    udop Tan = tan


class BOp a where
    bdop :: a -> (Double -> Double -> Double)

data BFunc = Add | Mul 
    deriving (Enum)

instance Show BFunc where
    show Add = "+"
    show Mul = "*"

instance BOp BFunc where
    bdop Add = (+)
    bdop Mul = (*)


-- Differentiate the given expression with respect to x(Var)

showExpr :: Expr -> String
showExpr (Num a)         = show a
showExpr (Var)           = "x"
showExpr (UnF f e)       = show f ++ "(" ++ showExpr e ++ ")"
showExpr (BiF Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
        where showFactor (BiF Add e1 e2) = "(" ++ showExpr (BiF Add e1 e2) ++ ")"
              showFactor e               = showExpr e
showExpr (BiF f e1 e2)   = showExpr e1 ++ " " ++ show f ++ " " ++ showExpr e2


eval :: Expr -> Double -> Double
eval (Num n) _       = n
eval (Var) v         = v
eval (UnF f e) v     = (udop f) (eval e v)
eval (BiF f e1 e2) v = (bdop f) (eval e1 v) (eval e2 v)


numVarParser :: Parser Expr
numVarParser  = fmap Num doubleParser +++ varParser

readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                     Just (e,"") -> Just e
                     _           -> Nothing

expr :: Parser Expr
expr = foldr1 (BiF Add) `fmap` chain term (char '+')

term :: Parser Expr
term = foldr1 (BiF Mul) `fmap` chain funcTerm (char '*')

funcTerm :: Parser Expr
funcTerm = foldl1 (+++) (map parseUF [(toEnum 0::UFunc)..])
       +++ factor

parseUF :: UFunc -> Parser Expr
parseUF f = do 
    (stringParser . show) f
    e <- factor
    return (UnF f e)

factor :: Parser Expr
factor = char '(' >-> expr <-< char ')' +++ numVarParser

-- Create a new parser for string like char m -> a (m -> [a])
-- Sequence

stringParser :: String -> Parser ()
stringParser s = sequence_ [ char c | c <- s]


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

-- Check lecture notes for simplify
simplify :: Expr -> Expr
simplify Var     = Var
simplify (Num x) = Num x
simplify (UnF f e) = (UnF f (simplify e))
simplify (BiF Add e1 e2) = case (BiF Add p q) of 
      (BiF Add (Num x) (Num y)) -> Num $ x+y
      (BiF Add (Num 0) q)       -> q
      (BiF Add p (Num 0))       -> p
      (BiF Add p q)             -> (BiF Add p q)
    where p = simplify e1
          q = simplify e2
simplify (BiF Mul e1 e2) = case (BiF Mul p q) of 
      (BiF Mul (Num x) (Num y)) -> Num $ x*y
      (BiF Mul (Num 0) _)       -> Num 0
      (BiF Mul _ (Num 0))       -> Num 0
      (BiF Mul (Num 1) q)       -> q
      (BiF Mul p (Num 1))       -> p 
      (BiF Mul p q)   -> (BiF Mul p q)
    where p = simplify e1
          q = simplify e2


-- Differentiate the given expression with respect to x(Var)
-- Using simplify to keep things nice and tidy after differentation.
differentiate :: Expr -> Expr
differentiate Var             = Num 1
differentiate (Num _)         = Num 0
differentiate (UnF Sin e)     = simplify $ BiF Mul (differentiate e) (UnF Cos e)
differentiate (UnF Cos e)     = simplify $ BiF Mul (Num (-1)) (BiF Mul (differentiate e) (UnF Sin e))
differentiate (BiF Add e1 e2) = simplify $ BiF Add (differentiate e1) (differentiate e2)
differentiate (BiF Mul e1 e2) = simplify $ BiF Add (BiF Mul (differentiate e1) e2) (BiF Mul e1 (differentiate e2))

-- Just a shortcut function so I can write expressions as Strings and try them out
readAndDiff :: String -> Expr
readAndDiff s = differentiate $ simplify $ fromJust $ readExpr s