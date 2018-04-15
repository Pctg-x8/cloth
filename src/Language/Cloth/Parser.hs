module Language.Cloth.Parser (Expr(..), factorExpr, infixExpr, Parser(runParser)) where

import Language.Cloth.Location
import qualified Language.Cloth.Tokenizer as Tok
import Language.Cloth.Tokenizer (Token)
import Control.Applicative
import Control.Arrow (first)
import Data.Text (Text)

data Expr = Var Text | Number Tok.NumberTok | Infix Expr Text Expr deriving (Eq, Show)

factorExpr, infixExpr :: Parser (Located Expr)
op :: Parser (Located Text)
factorExpr = Parser $ \ts -> case ts of
  ((Tok.Ident t :@: p) : tr) -> Right (Var t :@: p, tr)
  ((Tok.Number n :@: p) : tr) -> Right (Number n :@: p, tr)
  _ -> Left ts
infixExpr = (liftA3 Infix <$> factorExpr <*> op <*> infixExpr) <|> factorExpr
op = Parser $ \ts -> case ts of
  ((Tok.Backquote :@: _) : (Tok.Ident t :@: p) : (Tok.Backquote :@: _) : tr) -> Right (t :@: p, tr)
  ((Tok.Op t :@: p) : tr) -> Right (t :@: p, tr)
  _ -> Left ts

newtype Parser a = Parser { runParser :: [Located Token] -> Either [Located Token] (a, [Located Token]) }
instance Functor Parser where
  fmap f p = Parser $ \ts -> either Left (Right . first f) $ runParser p ts
instance Applicative Parser where
  pure v = Parser $ \ts -> Right (v, ts)
  pf <*> p1 = Parser $ \ts -> case runParser pf ts of
    Right (f, tr) -> runParser (f <$> p1) tr
    Left tr -> Left tr
instance Monad Parser where
  return = pure
  p1 >>= p2 = Parser $ \ts -> case runParser p1 ts of
    Right (v1, tr) -> runParser (p2 v1) tr
    Left tr -> Left tr
instance Alternative Parser where
  empty = Parser Left
  a <|> b = Parser $ \ts -> either (const $ runParser b ts) Right $ runParser a ts
