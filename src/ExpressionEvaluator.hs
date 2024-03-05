module ExpressionEvaluator (evalExpression) where

import ArithmeticExpression (Expression (Div, Mult, Pow, Sum, Val))

evalExpression :: Expression -> Int
evalExpression (Val n) = n
evalExpression (Sum e1 e2) = evalExpression e1 + evalExpression e2
evalExpression (Mult e1 e2) = evalExpression e1 * evalExpression e2
evalExpression (Pow e1 e2) = evalExpression e1 ^ evalExpression e2
evalExpression (Div e1 e2) = evalExpression e1 `div` evalExpression e2
