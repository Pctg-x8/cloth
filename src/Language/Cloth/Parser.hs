module Language.Cloth.Parser where

import Language.Cloth.Location
import Language.Cloth.Tokenizer
import Control.Applicative
import Control.Arrow (first)



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
