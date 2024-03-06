module ExpressionEvaluator (evalExpression, strToExpression) where

import ArithmeticExpression (Expression (..), priority)
import Data.Char (isDigit)

isNotDigit :: Char -> Bool
isNotDigit = not . isDigit

evalExpression :: Expression -> Int
evalExpression (Val e) = e
evalExpression (Pow e1 e2) = evalExpression e1 ^ evalExpression e2
evalExpression (Mult e1 e2) = evalExpression e1 * evalExpression e2
evalExpression (Div e1 e2) = evalExpression e1 `div` evalExpression e2
evalExpression (Sum e1 e2) = evalExpression e1 + evalExpression e2

operatorToExpression :: Char -> Expression -> Expression -> Expression
operatorToExpression c e1 e2
  | c == '^' = Pow e1 e2
  | c == '/' = Div e1 e2
  | c == '*' = Mult e1 e2
  | c == '+' = Sum e1 e2
  | otherwise = error "Invalid operator"

strToExpression :: String -> Maybe Expression
strToExpression [] = Nothing
strToExpression xs
  | null rest = Just num
  | otherwise =
      case strToExpression restAfterOp of
        Nothing -> Just num
        Just (Val e) -> Just (operatorToExpression op num (Val e))
        Just (Sum e1 e2) -> Just (Sum (operatorToExpression op num e1) e2)
        Just (Mult e1 e2) -> do
          if priority op >= priority '*'
            then Just (Mult (operatorToExpression op num e1) e2)
            else Just (operatorToExpression op num (Mult e1 e2))
        Just (Div e1 e2) ->
          if priority op >= priority '/'
            then Just (Div (operatorToExpression op num e1) e2)
            else Just (operatorToExpression op num (Div e1 e2))
        Just (Pow e1 e2) ->
          if priority op >= priority '^'
            then Just (Pow (operatorToExpression op num e1) e2)
            else Just (operatorToExpression op num (Pow e1 e2))
  where
    num = Val (read (takeWhile isDigit xs))
    rest = dropWhile isDigit xs
    op = head (takeWhile isNotDigit rest)
    restAfterOp = dropWhile isNotDigit rest
