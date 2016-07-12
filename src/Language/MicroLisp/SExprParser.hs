module Language.MicroLisp.SExprParser
    ( readSExpr
    , SExpr(..)
    ) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

data SExpr = SAtom String
           | SList [SExpr]
           | SInt Int

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser SExpr
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ SAtom atom

parseNumber :: Parser SExpr
parseNumber = (SInt . read) <$> many1 digit

parseSList :: Parser SExpr
parseSList = SList <$> sepBy parseExpr spaces

parseExpr :: Parser SExpr
parseExpr = parseAtom
            <|> parseNumber
            <|> do char '('
                   x <- try parseSList
                   char ')'
                   return x

-- | Parse an s-expression
readSExpr :: String -> Either ParseError SExpr
readSExpr input = parse parseExpr "SExpr" input
