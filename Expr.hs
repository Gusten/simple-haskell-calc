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
    | UnF UFunc Expr
    | BiF BFunc Expr Expr
        deriving (Show)


{--
  By defining a separate class and data type for the unary and binary 
  functions. We can have most of the function specific implementation
  in one place and also keep it easily extendable.
--}

-- Data type to represent unary functions
data UFunc = Sin | Cos | Tan
    deriving (Show, Enum)

-- This class lets us map desired implemented functions to each
-- of the defined unary functions.
class UOp a where
    udop :: a -> (Double -> Double)
    uStrRep :: a -> String

-- Definitions of each unary functions actual implementation and
-- their string representation. 
instance UOp UFunc where
    udop Sin = sin
    udop Cos = cos
    udop Tan = tan
    uStrRep Sin = "sin"
    uStrRep Cos = "cos"
    uStrRep Tan = "tan"

-- Data type to represent binary functions
-- The order of definition decides the priority of evaluation
data BFunc = Add | Mul 
    deriving (Eq, Show, Enum)

-- This class lets us map desired implemented functions to each
-- of the defined binary functions.
class BOp a where
    bdop :: a -> (Double -> Double -> Double)
    bStrRep :: a -> String

-- Definitions of each binary functions actual implementation and
-- their string representation. 
instance BOp BFunc where
    bdop Add   = (+)
    bdop Mul   = (*)
    bStrRep Add = "+"
    bStrRep Mul = "*"



-- Show a given expression
showExpr :: Expr -> String
showExpr (Num a)       = show a
showExpr (Var)         = "x"
showExpr (UnF f e)     = uStrRep f ++ "(" ++ showExpr e ++ ")"
-- This makes use of the order of the defined binary functions to handle
-- priority for evaluation.
showExpr (BiF f1 e1 e2) = showFactor e1 ++ " " ++ bStrRep f1 ++ " " ++ showFactor e2
        where showFactor (BiF f2 e1 e2) | i2 < i1 = "(" ++ showExpr (BiF f2 e1 e2) ++ ")"
                  where i1 = fromJust $ elemIndex f1 [(toEnum 0::BFunc)..]
                        i2 = fromJust $ elemIndex f2 [(toEnum 0::BFunc)..]
              showFactor e               = showExpr e


-- This function evaluates a given Expr using the given double as the Var value.
-- UnF functions/operators are extracted using udop and bdop respectively.
eval :: Expr -> Double -> Double
eval (Num n) _       = n
eval (Var) v         = v
eval (UnF f e) v     = (udop f) (eval e v)
eval (BiF f e1 e2) v = (bdop f) (eval e1 v) (eval e2 v)


numVarParser :: Parser Expr
numVarParser  = fmap Num doubleParser +++ varParser

readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s
             in case parse (expr [(toEnum 0::BFunc)..]) s' of
                     Just (e,"") -> Just e
                     _           -> Nothing


-- Function to try an parse an expression
-- This takes the list of binary functions as input so it can
-- try and parse the functions in an correct order.
-- Maybe this could be remade even more, I'm not sure tbh.
-- Especially the funcTerm part.
expr :: [BFunc] -> Parser Expr
expr (f:[]) = foldr1 (BiF f) `fmap` chain funcTerm (stringParser (bStrRep f))
      where funcTerm = foldl1 (+++) (map parseUF [(toEnum 0::UFunc)..])
                       +++ factor
expr (f:fs) = foldr1 (BiF f) `fmap` chain (expr fs) (stringParser (bStrRep f))

-- This is in my opinion ugly but I can't really say how to "prettify" it.
parseUF :: UFunc -> Parser Expr
parseUF f = do 
    (stringParser . uStrRep) f
    e <- factor
    return (UnF f e)

factor :: Parser Expr
factor = char '(' >-> (expr [(toEnum 0::BFunc)..]) <-< char ')' +++ numVarParser


-- Parse a string by using the char parser.
stringParser :: String -> Parser ()
stringParser s = sequence_ [ char c | c <- s]


varParser :: Parser Expr
varParser = do char 'x' 
               return Var


doubleParser :: Parser Double
doubleParser = do num <- readsP
                  return num


-- Simplify a given expression
-- I made this much smaller the second time around for code readability.
-- It's not as good but for the purpose of this lab it is ok.
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