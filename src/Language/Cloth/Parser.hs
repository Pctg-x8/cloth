{-# LANGUAGE OverloadedStrings #-}

module Language.Cloth.Parser (Expr(..), Pat(..), parseLayout, expr, pat, packageBlock, Parser(runParser)) where

import Language.Cloth.Location
import qualified Language.Cloth.Tokenizer as Tok
import Language.Cloth.Tokenizer (Token(..), KeywordKind(..))
import Control.Applicative
import Control.Arrow (first)
import Control.Monad (void)
import Data.Text (Text)

data Expr = Var Text | Num Tok.NumberTok | Infix Expr [Located (Text, Expr)] | Neg Expr | Apply Expr Expr |
  RightSection Text (Located Expr) | LeftSection Expr Text | Unit | Tuple [Located Expr] |
  List [Located Expr] | ArithmeticSeq (Located Expr) (Maybe (Located Expr)) (Maybe (Located Expr))
  deriving (Eq, Show)
data Pat = VarP Text | AsP Text (Located Pat) | NumP Tok.NumberTok
  deriving (Eq, Show)

data Lexeme = Tok Token | Bracketed | Angular
indent :: Located Lexeme -> Int
indent = left . location
-- |Generates hints for parsing the layout
lexemes :: [Located Token] -> [Located Lexeme]
lexemes ts = if all (/= (item $ head ts)) [LeftBrace, Keyword Package]
  then (Bracketed :@: location (head ts)) : lexemes' ts else lexemes' ts where
    lexemes' :: [Located Token] -> [Located Lexeme]
    lexemes' (t0 : t1 : ts)
      | any (== item t0) blockProvider && item t1 /= LeftBrace
        = (Tok <$> t0) : (Bracketed :@: if null ts then Location 0 0 else location t1) : lexemes' (t1 : ts)
      | line (location t0) /= line (location t1) && left (location t1) > 0
        = (Tok <$> t0) : (Angular :@: location t1) : lexemes' (t1 : ts)
    lexemes' (t : ts) = (Tok <$> t) : lexemes' ts
    lexemes' [] = []
    blockProvider = [Keyword Do, Keyword Where, Keyword Let, Keyword Of, Keyword Then, Keyword Else]
parseLayout :: [Located Token] -> [Located Token]
parseLayout = flip go [] . lexemes where
  go ((Angular :@: p) : ts) (m : ms)
    | left p == m = (Semicolon :@: p) : go ts (m : ms)
    | left p <  m = (RightBracket :@: p) : go ((Angular :@: p) : ts) ms
  go ((Angular :@: p) : ts) ms = go ts ms
  go ((Bracketed :@: p) : ts) (m : ms) | left p > m = (LeftBrace :@: p) : go ts (left p : m : ms)
  go ((Bracketed :@: p) : ts) [] | left p > 0 = (LeftBrace :@: p) : go ts [left p]
  go ((Bracketed :@: p) : ts) ms = (LeftBrace :@: p) : (RightBrace :@: p) : go ((Angular :@: p) : ts) ms
  go ((Tok RightBrace :@: p) : ts) (0 : ms) = (RightBrace :@: p) : go ts ms
  go ((Tok RightBrace :@: p) : ts) ms = error "Layout error"
  go ((Tok LeftBrace :@: p) : ts) ms = (LeftBrace :@: p) : go ts (0 : ms)
  go ((Tok t :@: p) : ts) (m : ms) | m /= 0 && False{-stub-} = (RightBrace :@: p) : go ((Tok t :@: p) : ts) ms
  go ((Tok t :@: p) : ts) ms = (t :@: p) : go ts ms
  go [] [] = []
  go [] (m : ms) | m /= 0 = (RightBrace :@: Location 0 m) : go [] ms

packageBlock :: Parser [Located Expr]
packageBlock = do
  decls <- match LeftBrace *> intersperse Semicolon expr
  match RightBrace *> return decls
factorExpr, infixExpr, applyExpr, expr :: Parser (Located Expr)
expr = infixExpr
factorExpr = Parser $ \ts -> case ts of
  ((Tok.Ident  t :@: p) : tr) -> Right (Var t :@: p, tr)
  ((Tok.LeftParenthese :@: p) : (Tok.Op t :@: _) : (Tok.RightParenthese :@: _) : tr) -> Right (Var t :@: p, tr)
  ((Tok.Number n :@: p) : tr) -> Right (Num n :@: p, tr)
  ((Tok.LeftParenthese :@: p) : tr') ->
    let
      sections = ((liftA2 LeftSection <$> infixExpr <*> op) <|> ((:@: p) <$> (RightSection <$> (item <$> op) <*> infixExpr)))
        <* match Tok.RightParenthese
      genTuple ts' = case ts' of [] -> Unit :@: p; [t] -> t; _ -> Tuple ts' :@: p
    in runParser ((<@> p) <$> (sections <|> ((genTuple <$> exprList) <* match Tok.RightParenthese))) tr'
  ((Tok.LeftBracket :@: p) : tr') ->
    let
      aseq = (ArithmeticSeq <$> expr <*> optv (match (Tok.Op ",") *> expr) <*> (match Tok.RangeOp *> optv expr)) <* match Tok.RightBracket
    in runParser ((:@: p) <$> (aseq <|> ((List <$> exprList) <* match Tok.RightBracket))) tr'
  _ -> Left ts
applyExpr = factorExpr >>= recurse where
  recurse :: Located Expr -> Parser (Located Expr)
  recurse lhs = ((liftA2 Apply lhs <$> factorExpr) >>= recurse) <|> return lhs
infixExpr = (liftA2 Infix <$> applyExpr <*> (pure <$> some (liftA2 (,) <$> op <*> applyExpr))) <|> (negop >> fmap Neg <$> applyExpr) <|> applyExpr

factorPat, pat :: Parser (Located Pat)
pat = factorPat
factorPat = Parser $ \ts -> case ts of
  ((Ident t :@: p) : (Tok.Op "@" :@: _) : tr) -> runParser ((:@: p) <$> (AsP t <$> pat)) tr
  ((Ident t :@: p) : tr) -> Right (VarP t :@: p, tr)
  ((Tok.Number n :@: p) : tr) -> Right (NumP n :@: p, tr)
  _ -> Left ts

exprList :: Parser [Located Expr]
exprList = opt $ intersperse (Tok.Op ",") expr

op :: Parser (Located Text)
negop :: Parser ()
op = Parser $ \ts -> case ts of
  ((Tok.Backquote :@: _) : (Tok.Ident t :@: p) : (Tok.Backquote :@: _) : tr) -> Right (t :@: p, tr)
  ((Tok.Op "," :@: _) : _) -> Left ts
  ((Tok.Op t :@: p) : tr) -> Right (t :@: p, tr)
  _ -> Left ts
negop = void $ match (Tok.Op "-")

newtype Parser a = Parser { runParser :: [Located Token] -> Either [Located Token] (a, [Located Token]) }
instance Functor Parser where
  fmap f p = Parser $ \ts -> either Left (Right . first f) $ runParser p ts
instance Applicative Parser where
  pure v = Parser $ \ts -> Right (v, ts)
  pf <*> p1 = Parser $ \ts -> runParser pf ts >>= (\(f, tr) -> runParser (f <$> p1) tr)
instance Monad Parser where
  return = pure
  p1 >>= p2 = Parser $ \ts -> runParser p1 ts >>= (\(v1, tr) -> runParser (p2 v1) tr)
instance Alternative Parser where
  empty = Parser Left
  a <|> b = Parser $ \ts -> either (const $ runParser b ts) Right $ runParser a ts

match :: Token -> Parser Location
match t = Parser $ \ts -> case ts of ((t' :@: p) : tr) | t' == t -> Right (p, tr); _ -> Left ts
optv :: (Applicative a, Alternative a) => Parser x -> Parser (a x)
-- |parse an alternative value, returns `empty` if failed
opt :: (Applicative a, Alternative a) => Parser (a x) -> Parser (a x)
-- | automatic promoted version of `opt`
optv = opt . fmap pure
opt p = p <|> pure empty
intersperse :: Token -> Parser a -> Parser [a]
intersperse tk p = ((:) <$> p <*> many (match tk *> p))
