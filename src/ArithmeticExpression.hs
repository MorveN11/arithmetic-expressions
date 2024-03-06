module ArithmeticExpression (Expression (..), priority) where

type Operator = Char

type Priority = Int

data Expression
  = Val Int
  | Pow Expression Expression
  | Mult Expression Expression
  | Div Expression Expression
  | Sum Expression Expression
  deriving (Show, Eq)

priority :: Operator -> Priority
priority c
  | c == '^' = 8
  | c == '/' = 7
  | c == '*' = 7
  | c == '+' = 6
  | otherwise = 0
