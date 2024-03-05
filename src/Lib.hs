module Lib
  ( mainLib,
  )
where

import ExpressionEvaluator (strToExpression)

mainLib :: IO ()
mainLib = do
  print (strToExpression "2+3")
  print (strToExpression "2*3")
  print (strToExpression "2/3")
  print (strToExpression "2^3")
  print (strToExpression "2+3+4")
  print (strToExpression "2*3*4")
  print (strToExpression "2*3+4")
  print (strToExpression "2+3*4")
