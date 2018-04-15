{-# LANGUAGE OverloadedStrings #-}

module Language.Cloth.Parser (Expr(..), factorExpr, infixExpr, applyExpr, Parser(runParser)) where

import Language.Cloth.Location
import qualified Language.Cloth.Tokenizer as Tok
import Language.Cloth.Tokenizer (Token)
import Control.Applicative
import Control.Arrow (first)
import Data.Text (Text)

data Expr = Var Text | Number Tok.NumberTok | Infix Expr Text Expr | Neg Expr | Apply Expr Expr deriving (Eq, Show)

factorExpr, infixExpr, applyExpr :: Parser (Located Expr)
op :: Parser (Located Text)
negop :: Parser ()
factorExpr = Parser $ \ts -> case ts of
  ((Tok.Ident  t :@: p) : tr) -> Right (Var t :@: p, tr)
  ((Tok.Number n :@: p) : tr) -> Right (Number n :@: p, tr)
  _ -> Left ts
applyExpr = factorExpr >>= recurse where
  recurse :: Located Expr -> Parser (Located Expr)
  recurse lhs = ((liftA2 Apply lhs <$> factorExpr) >>= recurse) <|> return lhs
infixExpr = (liftA3 Infix <$> applyExpr <*> op <*> infixExpr) <|> (negop >> fmap Neg <$> applyExpr) <|> applyExpr
op = Parser $ \ts -> case ts of
  ((Tok.Backquote :@: _) : (Tok.Ident t :@: p) : (Tok.Backquote :@: _) : tr) -> Right (t :@: p, tr)
  ((Tok.Op t :@: p) : tr) -> Right (t :@: p, tr)
  _ -> Left ts
negop = Parser $ \ts -> case ts of ((Tok.Op "-" :@: _) : tr) -> Right ((), tr); _ -> Left ts

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
