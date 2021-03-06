{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Language.Cloth.Tokenizer (
  tokenize, tokenizeAll, Located(..), Location(..), intoLocated, location, item, Token(..), NumberTok(..),
  KeywordKind(..), SpecialOps(..), PrimitiveTypes(..)
) where

import qualified Data.Text as T
import Data.Text
import Data.Char
import Control.Arrow (first)
import Control.Applicative (Alternative(..))
import Debug.Trace ()
import Language.Cloth.Location

data Token = Number NumberTok | Op Text | Ident Text | EOF | LeftParenthese | RightParenthese | Backquote |
  LeftBracket | RightBracket | LeftBrace | RightBrace | Semicolon | Keyword KeywordKind | Atmark |
  SpecialOp SpecialOps | PrimType PrimitiveTypes
  deriving (Show, Eq)
data KeywordKind = Where | Do | Let | In | Case | Of | While | For | Import | Package | Deriving |
  Class | Object | Trait | Struct | If | Then | Else
  deriving (Show, Eq)
data PrimitiveTypes = Int | Long | Float | Double | Bool | Char | Byte | Short | Word | DWord deriving (Show, Eq)
data SpecialOps = ArrowOp | MetaHintOp | RangeOp | EqualOp
  deriving (Show, Eq)
data NumberTok = Decimal Text (Maybe Text) | Hexadecimal Text (Maybe Text) |
  Binary Text (Maybe Text) | Octadecimal Text (Maybe Text) deriving (Show, Eq)
keywording :: Text -> Either Text Token
keywording t = case t of
  "where" -> Right $ Keyword Where
  "do" -> Right $ Keyword Do
  "does" -> Right $ Keyword Do
  "let" -> Right $ Keyword Let
  "in" -> Right $ Keyword In
  "case" -> Right $ Keyword Case
  "of" -> Right $ Keyword Of
  "while" -> Right $ Keyword While
  "for" -> Right $ Keyword For
  "import" -> Right $ Keyword Import
  "package" -> Right $ Keyword Package
  "deriving" -> Right $ Keyword Deriving
  "class" -> Right $ Keyword Class
  "object" -> Right $ Keyword Object
  "trait" -> Right $ Keyword Trait
  "struct" -> Right $ Keyword Struct
  "if" -> Right $ Keyword If
  "then" -> Right $ Keyword Then
  "else" -> Right $ Keyword Else
  "bool" -> Right $ PrimType Bool
  "byte" -> Right $ PrimType Byte
  "word" -> Right $ PrimType Word
  "short" -> Right $ PrimType Short
  "char" -> Right $ PrimType Char
  "dword" -> Right $ PrimType DWord
  "int" -> Right $ PrimType Int
  "long" -> Right $ PrimType Long
  "float" -> Right $ PrimType Float
  "double" -> Right $ PrimType Double
  _ -> Left t
specialOp :: Text -> Either Text SpecialOps
specialOp t = case t of
  "->" -> Right ArrowOp
  "::" -> Right MetaHintOp
  ".." -> Right RangeOp
  "=" -> Right EqualOp
  _ -> Left t

tokenize :: Located Text -> Maybe (Located Token, Located Text)
tokenize = either (const Nothing) Just . runCharParser tokparse
tokenizeAll :: Located Text -> [Located Token]
tokenizeAll t = case tokenize t of
  Just (EOF :@: _, _) -> []
  Just (tp, r) -> tp : tokenizeAll r
  Nothing -> error "Unable to tokenize the input"

tokparse :: CharParser (Located Token)
tokparse = CharParser $ \t -> case t of
  ((T.null -> True) :@: p) -> Right (EOF :@: p, t)
  ((T.stripPrefix "0x" . T.toLower -> Just tr) :@: p) -> runCharParser (nparse Hexadecimal isHexDigit) (tr :@: p)
  ((T.stripPrefix "0o" . T.toLower -> Just tr) :@: p) -> runCharParser (nparse Octadecimal isOctDigit) (tr :@: p)
  ((T.stripPrefix "0b" . T.toLower -> Just tr) :@: p) ->
    let isBinDigit x = x == '0' || x == '1' in runCharParser (nparse Binary isBinDigit) (tr :@: p)
  ((T.uncons -> Just (c, tr)) :@: p)
    | c == '\n' -> runCharParser tokparse (tr :@: advanceLine p)
    | isSpace c -> runCharParser tokparse (tr :@: advanceLeft p)
    | c == '#' -> runCharParser (parseUntilNorEmpty (== '\n') >> tokparse) (tr :@: advanceLeft p)
    | c == '`' -> return (Backquote :@: p, tr :@: advanceLeft p)
    | c == '(' -> return (LeftParenthese :@: p, tr :@: advanceLeft p)
    | c == ')' -> return (RightParenthese :@: p, tr :@: advanceLeft p)
    | c == '[' -> return (LeftBracket :@: p, tr :@: advanceLeft p)
    | c == ']' -> return (RightBracket :@: p, tr :@: advanceLeft p)
    | c == '{' -> return (LeftBrace :@: p, tr :@: advanceLeft p)
    | c == '}' -> return (RightBrace :@: p, tr :@: advanceLeft p)
    | c == ';' -> return (Semicolon :@: p, tr :@: advanceLeft p)
    | c == '@' -> return (Atmark :@: p, tr :@: advanceLeft p)
    | isDigit c -> runCharParser (nparse Decimal isDigit) t
    | isSymbolChar c ->
      runCharParser (fmap (either Op SpecialOp . specialOp) <$> parseWhile isSymbolChar) t
    | isLower c || isUpper c || c == '_' || c == '\'' ->
      runCharParser (fmap (either Ident id . keywording) <$>
        parseWhile (isLower <||> isUpper <||> isNumber <||> (== '_') <||> (== '\''))) t
    | otherwise -> Left t
  _ -> Left t
nparse :: (Text -> Maybe Text -> NumberTok) -> (Char -> Bool) -> CharParser (Located Token)
nparse nctor digitPredicate = fmap Number <$> (fparse <|> iparse) where
  fparse, iparse :: CharParser (Located NumberTok)
  fparse = (<*>) <$> (fmap nctor <$> parseWhile digitPredicate) <*> (fmap Just <$> (parseChar '.' *> parseWhile digitPredicate))
  iparse = fmap (flip nctor Nothing) <$> parseWhile digitPredicate
isSymbolChar :: Char -> Bool
isSymbolChar c = (T.any (== c) "!#$%&*+./<=>?@\\^-|~:" || isPunctuation c || isSymbol c) && not (T.any (== c) "()[]{}_")

-- Combining Predicates
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f <||> g = (||) <$> f <*> g

newtype CharParser a = CharParser { runCharParser :: Located Text -> Either (Located Text) (a, Located Text) }
instance Functor CharParser where
  fmap f p = CharParser $ \ts -> either Left (Right . first f) $ runCharParser p ts
instance Applicative CharParser where
  pure v = CharParser $ \ts -> Right (v, ts)
  pf <*> p1 = CharParser $ \ts -> case runCharParser pf ts of
    Right (f, tr) -> runCharParser (f <$> p1) tr
    Left tr -> Left tr
instance Monad CharParser where
  return = pure
  p1 >>= p2 = CharParser $ \ts -> case runCharParser p1 ts of
    Right (v1, tr) -> runCharParser (p2 v1) tr
    Left tr -> Left tr
instance Alternative CharParser where
  empty = CharParser Left
  a <|> b = CharParser $ \ts -> either (const $ runCharParser b ts) Right $ runCharParser a ts

parseChar :: Char -> CharParser Location
parseChar c = CharParser $ \input@(ts :@: p) -> case T.uncons ts of
  Just (c', tr) | c == c' -> Right (p, tr :@: advanceLeft p)
  _ -> Left input
parseWhile, parseUntil, parseUntilNorEmpty :: (Char -> Bool) -> CharParser (Located Text)
parseWhile f = CharParser $ \input@(ts :@: p) -> let (a, b) = T.span f ts in
  if T.null a then Left input else Right (a :@: p, b :@: (iterate advanceLeft p !! T.length a))
parseUntilNorEmpty f = CharParser $
  \input@(ts :@: p) -> if T.null ts then Right ("" :@: p, input) else runCharParser (parseUntil f) input
parseUntil f = parseWhile $ not . f
-- cpTrace :: String -> CharParser ()
-- cpTrace msg = CharParser $ \ts -> trace (msg ++ "[input " ++ show ts ++ "]") $ Right ((), ts)
