module Parser.Ast where

import qualified Scanner
import qualified Scanner.Token as Token
import Scanner.Token

indentLength = 2

showOperator :: Token.Operator -> String
showOperator (Not) = "!"
showOperator (Add) = "+"
showOperator (Sub) = "-"
showOperator (Mul) = "*"
showOperator (Div) = "/"
showOperator (Mod) = "%"
showOperator (Eq) = "=="
showOperator (Ne) = "!="
showOperator (Lt) = "<"
showOperator (Ge) = ">="
showOperator (Gt) = ">"
showOperator (Le) = "<="
showOperator (And) = "&&"
showOperator (Or) = "||"

data Expression
  = NumericLiteral Double
  | Variable String
  | UnaryOperation Token.Operator Expression
  | BinaryOperation Expression Token.Operator Expression
  deriving (Eq)

data Statement
  = Assignment String Expression
  | Input String
  | Output Expression
  | Block [Statement]
  | BranchOne Expression Statement
  | BranchTwo Expression Statement Statement
  | PreTestLoop Expression Statement
  | PostTestLoop Expression Statement
  deriving (Eq)

indentString :: Int -> String -> String
indentString 0 s = s
indentString i s = (replicate (i * indentLength) ' ') ++ s

indentExpression :: Int -> Expression -> String
indentExpression i (NumericLiteral n) = indentString i $ show n
indentExpression i (Variable v) = indentString i v
indentExpression i (UnaryOperation op e) = indentString i $ showOperator op ++ show e
indentExpression i (BinaryOperation a op b) = indentString i $ show a ++ " " ++ showOperator op ++ " " ++ show b

instance Show Expression where
  show :: Expression -> String
  show e = indentExpression 0 e

indentStatement :: Int -> Statement -> String
indentStatement i (Assignment s e) = indentString i $ s ++ " = " ++ show e ++ ";\n"
indentStatement i (Input s) = indentString i $ "scan " ++ s ++ ";\n"
indentStatement i (Output e) = indentString i $ "print " ++ show e ++ ";\n"
indentStatement i (Block ss) = foldl (++) [] (map (indentStatement $ i + 1) ss)
indentStatement i (BranchOne e s) = indentString i $ "if (" ++ show e ++ ") {\n" ++ indentStatement (i + 1) s ++ indentString i "}\n"
indentStatement i (BranchTwo e s1 s2) = indentString i $ "if (" ++ show e ++ ") {\n" ++ indentStatement (i + 1) s1 ++ indentString i "} else {\n" ++ indentStatement (i + 1) s2 ++ indentString i "}\n"
indentStatement i (PreTestLoop e s) = indentString i $ "while (" ++ show e ++ ") {\n" ++ indentStatement (i + 1) s ++ indentString i "}\n"
indentStatement i (PostTestLoop e s) = indentString i $ "do {\n" ++ indentStatement (i + 1) s ++ "} while (" ++ show e ++ ")\n"

instance Show Statement where
  show :: Statement -> String
  show s = indentStatement (-1) s  -- kludge so that we print out *programs* with 0 indentation
