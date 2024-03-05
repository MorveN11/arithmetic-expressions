module ExpressionEvaluator (evalExpression, strToExpression) where

import ArithmeticExpression (Expression (..), priority)
import Data.Char (isDigit)

evalExpression :: Expression -> Int
evalExpression Nill = 0
evalExpression (Val e) = e
evalExpression (Pow e1 e2) = evalExpression e1 ^ evalExpression e2
evalExpression (Mult e1 e2) = evalExpression e1 * evalExpression e2
evalExpression (Div e1 e2) = evalExpression e1 `div` evalExpression e2
evalExpression (Sum e1 e2) = evalExpression e1 + evalExpression e2

operatorToExpression :: Char -> Expression -> Expression -> Expression
operatorToExpression c e1 e2
  | c == '^' = Pow e1 e2
  | c == '*' = Mult e1 e2
  | c == '/' = Div e1 e2
  | c == '+' = Sum e1 e2
  | otherwise = Nill

expressionToOperator :: Expression -> Char
expressionToOperator Nill = ' '
expressionToOperator (Val _) = ' '
expressionToOperator (Pow _ _) = '^'
expressionToOperator (Mult _ _) = '*'
expressionToOperator (Div _ _) = '/'
expressionToOperator (Sum _ _) = '+'

strToExpression :: String -> Expression
strToExpression [] = Nill
strToExpression xs
  | null rest = Val (read num)
  | otherwise =
      case strToExpression restAfterOp of
        Nill -> strToExpression num
        (Val e) -> operatorToExpression op (strToExpression num) (Val e)
        (Sum e1 e2) -> Sum (operatorToExpression op (strToExpression num) e1) e2
        (Mult e1 e2) -> do
          if priority op >= priority (expressionToOperator (Mult e1 e2))
            then Mult (operatorToExpression op (strToExpression num) e1) e2
            else operatorToExpression op (strToExpression num) (Mult e1 e2)
        (Div e1 e2) ->
          if priority op >= priority (expressionToOperator (Div e1 e2))
            then Div (operatorToExpression op (strToExpression num) e1) e2
            else operatorToExpression op (strToExpression num) (Div e1 e2)
        (Pow e1 e2) ->
          if priority op >= priority (expressionToOperator (Pow e1 e2))
            then Pow (operatorToExpression op (strToExpression num) e1) e2
            else operatorToExpression op (strToExpression num) (Pow e1 e2)
  where
    num = takeWhile isDigit xs
    rest = dropWhile isDigit xs
    op = head rest
    restAfterOp = tail rest
