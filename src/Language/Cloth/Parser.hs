{-# LANGUAGE OverloadedStrings #-}

module Language.Cloth.Parser (Expr(..), Pat(..), Stmt(..), parseLayout, expr, pat, packageBlock, Parser(runParser)) where

import Language.Cloth.Location
import qualified Language.Cloth.Tokenizer as Tok
import Language.Cloth.Tokenizer (Token(..), KeywordKind(..), NumberTok, SpecialOps(..))
import Control.Applicative
import Control.Arrow (first)
import Control.Monad (void)
import Data.Text (Text)
import Debug.Trace (trace)

data Expr = Var Text | Num NumberTok | Infix Expr [Located (Text, Expr)] | Neg Expr | Apply Expr Expr |
  RightSection Text (Located Expr) | LeftSection Expr Text | Unit | Tuple [Located Expr] |
  List [Located Expr] | ArithmeticSeq (Located Expr) (Maybe (Located Expr)) (Maybe (Located Expr)) |
  DoBlock [Located Stmt] | Conditional (Located Expr) [Located Stmt] [Located Stmt]
  deriving (Eq, Show)
data DataConstructor = DataConstructor Text | ListConstructor | TupleConstructor Int deriving (Eq, Show)

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
    blockProvider = [Keyword Do, Keyword Where, Keyword Of, Keyword Then, Keyword Else]
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
  go ((Tok t :@: p) : ts) (m : ms) | m /= 0 && any (== t) blockTerm = (RightBrace :@: p) : go ((Tok t :@: p) : ts) ms
  go ((Tok t :@: p) : ts) ms = (t :@: p) : go ts ms
  go [] [] = []
  go [] (m : ms) | m /= 0 = (RightBrace :@: Location 0 m) : go [] ms
  blockTerm = [Keyword Else]

packageBlock :: Parser [Located Stmt]
packageBlock = do
  decls <- match LeftBrace *> intersperse Semicolon stmt
  match RightBrace *> return decls

data Ty = IdentT Text | ApplyT Ty Ty deriving (Show, Eq)
data Stmt = ValueExpr Expr | Binding (Located (Pat, Maybe Ty, Expr)) deriving (Show, Eq)
stmt :: Parser (Located Stmt)
stmt = lettings <|> (fmap ValueExpr <$> expr) where
  lettings = do
    firstLocation <- match (Keyword Tok.Let)
    pat' :@: p <- pat
    expr' :@: _ <- match (SpecialOp EqualOp) *> expr
    return (Binding ((pat', Nothing, expr') :@: p) :@: firstLocation)
factorExpr, infixExpr, cExpr, applyExpr, expr :: Parser (Located Expr)
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
      tuplecons = genTuple <$> (opt $ intersperse (Op ",") expr)
    in runParser ((<@> p) <$> (sections <|> (tuplecons <* match Tok.RightParenthese))) tr'
  ((Tok.LeftBracket :@: p) : tr') ->
    let
      aseq = (ArithmeticSeq <$> expr <*> optv (match (Tok.Op ",") *> expr) <*> (match (SpecialOp RangeOp) *> optv expr)) <* match Tok.RightBracket
      list = List <$> (opt $ intersperse (Op ",") expr)
    in runParser ((:@: p) <$> (aseq <|> (list <* match Tok.RightBracket))) tr'
  _ -> Left ts
applyExpr = factorExpr >>= recurse where
  recurse lhs = ((liftA2 Apply lhs <$> factorExpr) >>= recurse) <|> return lhs
infixExpr = ((\(e, ps) -> Infix <$> e <*> pure [(a, b) :@: p | ((a :@: p), (b :@: _)) <- ps]) <$> intercalate1 op cExpr)
  <|> (negop >> fmap Neg <$> cExpr) <|> cExpr
cExpr = Parser $ \ts -> case ts of
  ((Tok.Keyword Do :@: p) : tr) -> runParser ((:@: p) <$> DoBlock <$> blocked stmt) tr
  ((Tok.Keyword If :@: p) : tr) -> runParser
    ((:@: p) <$> (Conditional <$> expr <*> (match (Keyword Then) *> blocked stmt) <*> opt (match (Keyword Else) *> blocked stmt)))
    tr
  _ -> runParser applyExpr ts

data Pat = VarP Text | AsP Text (Located Pat) | NumP NumberTok | ListP [Located Pat] | UnitP |
  TupleP [Located Pat] | WildcardP | DataP DataConstructor [Located Pat] | NegativeNumP Tok.NumberTok |
  InfixP Pat [Located (Text, Pat)]
  deriving (Eq, Show)
factorPat, prefixPat, infixPat, pat :: Parser (Located Pat)
pat = infixPat
infixPat = ((\(e, ps) -> InfixP <$> e <*> pure [(a, b) :@: p | ((a :@: p), (b :@: _)) <- ps]) <$> intercalate1 op prefixPat) <|> prefixPat
prefixPat = negativePat <|> maybeDataPat where
  negativePat = liftA NegativeNumP <$> (match (Op "-") *> num)
  maybeDataPat = Parser ps where
    ps ts = runParser (pp ts) ts
    pp ts = case ts of
      ((Ident _ :@: _) : (Atmark :@: _) : tr) -> factorPat
      _ -> (liftA2 dataP <$> ctor <*> (pure <$> many factorPat)) <|> factorPat
    dataP (DataConstructor "_") [] = WildcardP
    dataP (DataConstructor c) [] = VarP c
    dataP a b = DataP a b
factorPat = Parser $ \ts -> case ts of
  ((Ident "_" :@: _) : (Atmark :@: _) : tr) -> runParser factorPat tr
  ((Ident t :@: p) : (Atmark :@: _) : tr) -> runParser ((:@: p) <$> (AsP t <$> factorPat)) tr
  ((Ident "_" :@: p) : tr) -> Right (WildcardP :@: p, tr)
  ((Ident t :@: p) : tr) -> Right (VarP t :@: p, tr)
  ((Tok.Number n :@: p) : tr) -> Right (NumP n :@: p, tr)
  ((LeftParenthese :@: p) : tr') ->
    let
      genTuple ts' = case ts' of [] -> UnitP :@: p; [t] -> t; _ -> TupleP ts' :@: p
    in runParser ((<@> p) <$> (genTuple <$> (opt $ intersperse (Op ",") pat)) <* match RightParenthese) tr'
  ((LeftBracket :@: p) : tr') -> let listPat = ListP <$> (opt $ intersperse (Op ",") pat) in
    runParser ((:@: p) <$> (listPat <* match RightBracket)) tr'
  _ -> Left ts

ctor :: Parser (Located DataConstructor)
ctor = Parser $ \ts -> case ts of
  ((Ident t :@: p) : tr) -> Right (DataConstructor t :@: p, tr)
  ((LeftBracket :@: p) : (RightBracket :@: _) : tr) -> Right (ListConstructor :@: p, tr)
  ((LeftParenthese :@: p) : tr) ->
    runParser ((:@: p) <$> (TupleConstructor . length) <$> some (match $ Op ",") <* match RightParenthese) tr
  _ -> Left ts

num :: Parser (Located NumberTok)
num = Parser $ \ts -> case ts of
  ((Tok.Number n :@: p) : tr) -> Right (n :@: p, tr)
  _ -> Left ts
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
intercalate, intercalate1 :: Parser a -> Parser b -> Parser (b, [(a, b)])
intercalate a b = (,) <$> b <*> many ((,) <$> a <*> b)
intercalate1 a b = (,) <$> b <*> some ((,) <$> a <*> b)
blocked :: Parser a -> Parser [a]
blocked p = match LeftBrace *> intersperse Semicolon p <* match RightBrace
parserTrace :: Show s => s -> Parser ()
parserTrace head = Parser $ \ts -> trace (show head ++ show ts) $ return ((), ts)
