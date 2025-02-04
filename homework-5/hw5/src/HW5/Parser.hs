module HW5.Parser (
    parse
  ) where

import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.Text (pack)
import Data.Void (Void)
import Numeric (readHex)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), ParseErrorBundle, Parsec,
  between, choice, some, empty, many, manyTill, runParser, satisfy, sepBy, sepEndBy)
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import Control.Monad (void)
import Control.Monad.Combinators (count)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.ByteString as BS
import HW5.Base (HiValue (..), HiFun (..), HiAction (..), HiExpr (..))

-- | The entry point for parsing a Hi expression from a string.
-- Returns either a parse error or the parsed expression.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between skipSpaces eof parse') ""

-- | Type alias for the parser.
type Parser = Parsec Void String

-- | The main parsing function, building the parse tree.
parse' :: Parser HiExpr
parse' = makeExprParser (choice [HiExprValue <$> makeLexeme pValue, brackets parse', pDict, pList]) tableBinaryOperators

-- | Parses arguments enclosed in brackets as a list of expressions.
pArgs :: Parser [HiExpr]
pArgs = brackets (parse' `sepBy` makeSymbol ",")

-- | Parses a dot-access operation as a list containing a string value.
pAccessViaDot :: Parser [HiExpr]
pAccessViaDot = do
  void $ char '.'
  str <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy` char '-'
  return [HiExprValue $ HiValueString (pack $ intercalate "-" str)]

-- | Parses a numeric value and converts it to a 'HiValue' of type 'HiValueNumber'.
pNumber :: Parser HiValue
pNumber = HiValueNumber . toRational <$> L.signed skipSpaces L.scientific

-- | Parses a function name and returns a 'HiValue' of type 'HiValueFunction'.
pFun :: Parser HiValue
pFun = HiValueFunction <$> choice (map (\fun -> fun <$ string (show fun)) [HiFunDiv ..])

-- | Parses a boolean value as a 'HiValue'.
pBool :: Parser HiValue
pBool = choice [HiValueBool True <$ string "true", HiValueBool False <$ string "false"]

-- | Parses the null value as a 'HiValue'.
pNull :: Parser HiValue
pNull = HiValueNull <$ string "null"

-- | Parses a string enclosed in double quotes as a 'HiValue'.
pString :: Parser HiValue
pString = HiValueString . pack <$> (char '\"' >> manyTill L.charLiteral (char '\"'))

-- | Parses a sequence of hex values enclosed in '[#' and '#]' and converts them to 'HiValueBytes'.
pBytes :: Parser HiValue
pBytes = do
  values <- betweenSymbols "[#" "#]" (pByte `sepEndBy` space1)
  return $ HiValueBytes (BS.pack values)
    where
      pByte = do
        chars <- count 2 hexDigitChar
        case readHex chars of
          [(value, "")]
            | value <= 255 -> return value
            | otherwise    -> empty
          _ -> empty

-- | Parses an actions "cwd" and "now" and returns a 'HiValueAction'.
pAction :: Parser HiValue
pAction = HiValueAction <$> choice [HiActionCwd <$ string "cwd", HiActionNow <$ string "now"]

-- | Parses a value, which can be a boolean, function, number, null, string, bytes, or action.
pValue :: Parser HiValue
pValue = choice [pBool, pFun, pNumber, pNull, pString, pBytes, pAction]

-- | Parses a list expression and returns it as a 'HiExpr' with an applied 'HiFunList' function.
pList :: Parser HiExpr
pList = HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> betweenSymbols "[" "]" (parse' `sepBy` makeSymbol ",")

-- | Parses a dictionary and returns it as a 'HiExpr' with key-value pairs.
pDict :: Parser HiExpr
pDict = HiExprDict <$> betweenSymbols "{" "}" (pDictElem `sepBy` makeSymbol ",")
  where
    pDictElem = (,) <$> (parse' <* makeSymbol ":") <*> parse'

-- | The operator table for binary operators.
tableBinaryOperators :: [[Operator Parser HiExpr]]
tableBinaryOperators = [
    [ callChain ]

  , [ infixL "*" HiFunMul
    , infix' InfixL (notFollowed "/" "=") HiFunDiv ]

  , [ infixL "+" HiFunAdd
    , infixL "-" HiFunSub ]

  , [ infixN "==" HiFunEquals
    , infixN "/=" HiFunNotEquals
    , infixN "<=" HiFunNotGreaterThan
    , infixN ">=" HiFunNotLessThan
    , infixN "<" HiFunLessThan
    , infixN ">" HiFunGreaterThan ]

  , [ infixR "&&" HiFunAnd ]

  , [ infixR "||" HiFunOr ]
  ]
  where
    infix' :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr)
      -> Parser String
      -> HiFun
      -> Operator Parser HiExpr
    infix' constructor symbol fun = constructor (makeExpr <$ symbol)
      where
        makeExpr a b = HiExprApply (HiExprValue $ HiValueFunction fun) [a, b]

    infixL, infixR, infixN :: String -> HiFun -> Operator Parser HiExpr
    infixL sign = infix' InfixL $ makeSymbol sign
    infixR sign = infix' InfixR $ makeSymbol sign
    infixN sign = infix' InfixN $ makeSymbol sign

    -- | Ensures the given symbol is not followed by another symbol.
    notFollowed :: String -> String -> Parser String
    notFollowed symbol next = makeLexeme . try $ string symbol <* notFollowedBy (makeSymbol next)

    -- | Defines a postfix operator for method chaining and function calls.
    callChain :: Operator Parser HiExpr
    callChain = Postfix $ buildCallChain <$> parseSteps
      where
        parseSteps :: Parser [Either [HiExpr] ()]
        parseSteps = some $ choice
          [ Left <$> pArgs
          , Left <$> pAccessViaDot
          , Right () <$ makeSymbol "!" ]

        buildCallChain :: [Either [HiExpr] ()] -> (HiExpr -> HiExpr)
        buildCallChain = foldr applyStep id

        applyStep :: Either [HiExpr] () -> (HiExpr -> HiExpr) -> (HiExpr -> HiExpr)
        applyStep (Left args) next = \cur -> next (HiExprApply cur args)
        applyStep (Right _) next   = next . HiExprRun

-- | Helper function to create a parser that matches a string enclosed by specified symbols.
betweenSymbols :: String -> String -> Parser a -> Parser a
betweenSymbols begin end = between (makeSymbol begin) (makeSymbol end)

-- | Helper function to parse expressions enclosed in parentheses.
brackets :: Parser a -> Parser a
brackets = betweenSymbols "(" ")"

-- | Helper function to create a parser that discards trailing whitespace.
makeLexeme :: Parser a -> Parser a
makeLexeme = L.lexeme skipSpaces

-- | Helper function to create a parser that matches a specific string.
makeSymbol :: String -> Parser String
makeSymbol = L.symbol skipSpaces

-- | Helper function to skip spaces in the input.
skipSpaces :: Parser ()
skipSpaces = L.space space1 empty empty
