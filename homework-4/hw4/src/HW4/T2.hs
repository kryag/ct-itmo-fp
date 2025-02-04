{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace, digitToInt)

import HW4.Types
import HW4.T1 (ExceptState(..))

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) input = case runES es (0, input) of
  Error e          -> Error e
  Success (a :# _) -> Success a

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  P p1 <|> P p2 = P $ ES $ \(pos, s) ->
    case runES p1 (pos, s) of
      Error _   -> runES p2 (pos, s)
      success   -> success

-- No methods
instance MonadPlus Parser

pWhitespace :: Parser ()
pWhitespace = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    (c:cs) -> if isSpace c
              then case runP pWhitespace cs of
                     Error _   -> Success (():# (pos, s))
                     Success _ -> Success (():# (pos + 1, cs))
              else Success (():# (pos, s))

pInt :: Parser Integer
pInt = do
  str <- some (mfilter isDigit pChar)
  return (foldl (\acc c -> acc * 10 + toInteger (digitToInt c)) 0 str)

pDouble :: Parser Double
pDouble = do
  whole <- some (mfilter isDigit pChar)
  _ <- mfilter (== '.') pChar
  frac <- some (mfilter isDigit pChar)
  let wholePart     = foldl (\acc c -> acc * 10 + toInteger (digitToInt c)) 0 whole
      fracPart      = foldl (\acc c -> acc * 10 + toInteger (digitToInt c)) 0 frac
      fracLength    = length frac
      rationalValue = toRational wholePart + toRational fracPart / (10 ^ fracLength)
  return $ fromRational rationalValue

pNumber :: Parser Double
pNumber = pDouble <|> (fromIntegral <$> pInt)

pFactorOp :: Parser (Expr -> Expr -> Expr)
pFactorOp = (mfilter (== '*') pChar >> pure (\x y -> Op (Mul x y)))
        <|> (mfilter (== '/') pChar >> pure (\x y -> Op (Div x y)))

pTermOp :: Parser (Expr -> Expr -> Expr)
pTermOp = (mfilter (== '+') pChar >> pure (\x y -> Op (Add x y)))
       <|> (mfilter (== '-') pChar >> pure (\x y -> Op (Sub x y)))

pFactor :: Parser Expr
pFactor = do
  pWhitespace
  val <- (Val <$> pNumber) <|> (mfilter (== '(') pChar >> pExpr <* mfilter (== ')') pChar)
  pWhitespace
  return val

pTerm :: Parser Expr
pTerm = chainLeftAssoc pFactor pFactorOp

chainLeftAssoc :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeftAssoc p op = do
  firstOperand <- p
  parseRest firstOperand
  where
    parseRest x = (do
                    operation <- op
                    secondOperand <- p
                    let result = operation x secondOperand
                    parseRest result
                 ) <|> pure x

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  if null s then Success (() :# (pos, s))
  else Error (ErrorAtPos pos)

pExpr :: Parser Expr
pExpr = chainLeftAssoc pTerm pTermOp

parse :: Parser Expr
parse = do
  expr <- pExpr
  _ <- pEof
  return expr

parseExpr :: String -> Except ParseError Expr
parseExpr = runP parse
