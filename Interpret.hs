module Interpret where

import Data.Map (Map, (!), insert)
import Scanner
import Parser
import Parser.Ast
import MoreIO
import Control.Monad.State

type Context = Map String Double

fmod :: Double -> Double -> Double
fmod x y = x - y * fromIntegral (truncate (x / y))

toBool :: Double -> Bool
toBool 0.0 = False
toBool _ = True

fromBool :: Bool -> Double
fromBool False = 0.0
fromBool True = 1.0

evaluate :: Expression -> Context -> Double
evaluate (NumericLiteral n) c = n
evaluate (Variable v) c = (c ! v)
evaluate (UnaryOperation Sub n) c = -(evaluate n c)
evaluate (UnaryOperation Not (NumericLiteral 0.0)) c = 1.0
evaluate (UnaryOperation Not (NumericLiteral _)) c = 0.0
evaluate (UnaryOperation Not e) c = evaluate (UnaryOperation Not (NumericLiteral $ evaluate e c)) c
evaluate (BinaryOperation a Add b) c = (evaluate a c) + (evaluate b c)
evaluate (BinaryOperation a Sub b) c = (evaluate a c) - (evaluate b c)
evaluate (BinaryOperation a Mul b) c = (evaluate a c) * (evaluate b c)
evaluate (BinaryOperation a Div b) c = (evaluate a c) / (evaluate b c)
evaluate (BinaryOperation a Mod b) c = fmod (evaluate a c) (evaluate b c)
evaluate (BinaryOperation a Eq b) c = fromBool $ (evaluate a c) == (evaluate b c)
evaluate (BinaryOperation a Ne b) c = fromBool $ (evaluate a c) /= (evaluate b c)
evaluate (BinaryOperation a Lt b) c = fromBool $ (evaluate a c) < (evaluate b c)
evaluate (BinaryOperation a Gt b) c = fromBool $ (evaluate a c) > (evaluate b c)
evaluate (BinaryOperation a Le b) c = fromBool $ (evaluate a c) <= (evaluate b c)
evaluate (BinaryOperation a Ge b) c = fromBool $ (evaluate a c) >= (evaluate b c)
evaluate (BinaryOperation a And b) c = if toBool (evaluate a c) && toBool (evaluate b c) then 1.0 else 0.0
evaluate (BinaryOperation a Or b) c = if toBool (evaluate a c) || toBool (evaluate b c) then 1.0 else 0.0

execute :: Statement -> StateT Context IO ()
execute (Assignment v e) = do
  n <- gets $ evaluate e
  modify (insert v n)
execute (Input v) = do
  n <- lift $ promptNumber v
  modify $ insert v n
execute (Output e) = do
  n <- gets $ evaluate e
  lift $ print n
execute (Block ss) = do
  mapM execute ss
  return ()
execute (BranchOne e s) = do
  b <- gets $ evaluate e
  if (toBool b)
    then execute s
  else return ()
execute (BranchTwo e s1 s2) = do
  b <- gets $ evaluate e
  if (toBool b)
    then execute s1
  else execute s2
execute l@(PreTestLoop e s) = do
  b <- gets $ evaluate e
  if (toBool b)
    then do
      execute s
      execute l
  else return ()
execute l@(PostTestLoop e s) = do
  execute s
  b <- gets $ evaluate e
  if (toBool b)
    then execute l
  else return ()

