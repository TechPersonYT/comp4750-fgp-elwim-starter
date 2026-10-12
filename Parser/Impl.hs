module Parser.Impl (parse) where

import Control.Applicative (many, some, optional, (<|>))

import Parser.Precedence
import Parser.Ast as Ast

import qualified Stream
import qualified Source
import qualified Scanner
import Scanner (Token(..), PosToken(..), Operator(..), TokenItr)


grabString :: Token -> String
grabString (Identifier s) = s

-- Token helper functions
isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier _ = False

isNumber :: Token -> Bool
isNumber (Number _) = True
isNumber _ = False

isUnaryOperator :: Token -> Bool
isUnaryOperator (Operator op) = isUnary op
isUnaryOperator _ = False



type Parser = Stream.Processor Scanner.TokenItr


parseToken :: Token -> Parser Token
parseToken = Stream.satisfy . (==)


requireToken :: Token -> Parser Token
requireToken token = Stream.require (parseToken token) ("expected: " ++ show token)


requirePrimary :: Parser Expression
requirePrimary = Stream.require parsePrimary "expected expression"


requireExpression :: Parser Expression
requireExpression = Stream.require parseExpr "expected expression"


requireStatement :: Parser Statement
requireStatement = Stream.require parseStatement "expected statement"


parseVariable :: Parser Expression
parseVariable = do
  token <- Stream.satisfy isIdentifier
  return $ Variable $ grabString token


parseNumber :: Parser Expression
parseNumber = do
  token <- Stream.satisfy isNumber
  return $ NumericLiteral $ (read $ show token :: Double)


parseUnary :: Parser Expression
parseUnary = do
  opToken <- Stream.satisfy isUnaryOperator
  expr <- requirePrimary
  return $ UnaryOperation (op opToken) expr where
    op (Operator o) = o


parseNested :: Parser Expression
parseNested = do
  parseToken LeftParen
  expr <- requireExpression
  requireToken RightParen
  return expr


parsePrimary :: Parser Expression
parsePrimary = parseNumber <|> parseVariable <|> parseUnary <|> parseNested


parseExpr :: Parser Expression
parseExpr = do
  expr <- parsePrimary
  moreOperators expr 0
  where
    moreOperators lhs minPrecedence = do
      next <- optional Stream.peekNext -- don't consume yet
      case next of
        Just (Operator op)
          | isBinary op && precedenceOf op > minPrecedence -> do
              Stream.getNext -- now we can consume
              rhs <- requirePrimary
              rhsExpanded <- moreOperators rhs (precedenceOf op)
              moreOperators (BinaryOperation lhs op rhsExpanded) minPrecedence
        _ -> return lhs


parsePrint :: Parser Statement
parsePrint = do
  parseToken Print
  expr <- requireExpression
  requireToken Semicolon
  return $ Output expr


parseScan :: Parser Statement
parseScan = do
  parseToken Scan
  var <- Stream.require (Stream.satisfy isIdentifier) "expected variable"
  requireToken Semicolon
  return $ Input $ grabString var


parseAssignment :: Parser Statement
parseAssignment = do
  var <- Stream.satisfy isIdentifier
  requireToken Assign
  value <- requireExpression
  requireToken Semicolon
  return $ Assignment (grabString var) value


parseBlock :: Parser Statement
parseBlock = do
  parseToken LeftBrace
  stmts <- many parseStatement
  requireToken RightBrace
  return $ Block stmts


parseIf :: Parser Statement
parseIf = do
  parseToken If
  requireToken LeftParen
  expr <- requireExpression
  requireToken RightParen
  thenStmt <- requireStatement
  maybeElse <- optional parseElse
  case maybeElse of
    Just elseStmt -> return $ BranchTwo expr thenStmt elseStmt
    Nothing       -> return $ BranchOne expr thenStmt
  where
    parseElse = do
      parseToken Else
      requireStatement


parseWhile :: Parser Statement
parseWhile = do
  parseToken While
  requireToken LeftParen
  expr <- requireExpression
  requireToken RightParen
  stmt <- requireStatement
  return $ PreTestLoop expr stmt


parseDoWhile :: Parser Statement
parseDoWhile = do
  parseToken Do
  stmt <- requireStatement
  requireToken While
  requireToken LeftParen
  expr <- requireExpression
  requireToken RightParen
  requireToken Semicolon
  return $ PostTestLoop expr stmt


parseStatement :: Parser Statement
parseStatement = parsePrint <|> parseScan <|> parseAssignment <|> parseBlock <|> parseIf <|> parseWhile <|> parseDoWhile


parseAll :: Parser Statement
parseAll = do
  statements <- many parseStatement
  eof <- Stream.getEof
  if eof
    then return $ Block statements
    else do
      token <- Stream.peekNext
      fail $ "unexpected: " ++ show token


parse :: [PosToken] -> Either (Source.Position, String) Statement
parse tokens = Stream.processResult parseAll (Scanner.begin tokens)
