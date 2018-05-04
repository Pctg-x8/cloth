{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Language.Cloth.Tokenizer as Tok
import Language.Cloth.Tokenizer
import qualified Language.Cloth.Parser as P
import Language.Cloth.Parser
import Data.Default (def)
import Data.Either (isRight)
import Data.Text

main :: IO ()
main = hspec $ do
  describe "Tokenizer" $ do
    it "advances line by LF" $ snd <$> tokenize (intoLocated "\n") `shouldBe` Just ("" :@: Location 2 1)
    it "leaves trailing correctly" $ snd <$> tokenize (intoLocated "3 ") `shouldBe` Just (" " :@: Location 1 2)
    it "parses Int" $
      item . fst <$> tokenize (intoLocated "36 ") `shouldBe` Just (Tok.Number $ Decimal "36" Nothing)
    it "parses Float" $
      item . fst <$> tokenize (intoLocated "36.02") `shouldBe` Just (Tok.Number $ Decimal "36" $ Just "02")
    it "parses Hexadecimals" $
      item . fst <$> tokenize (intoLocated "0x3a") `shouldBe` Just (Tok.Number $ Hexadecimal "3a" Nothing)
    it "parses Octadecimals" $
      item . fst <$> tokenize (intoLocated "0o7.5") `shouldBe` Just (Tok.Number $ Octadecimal "7" $ Just "5")
    it "parses Binary Literals" $
      item . fst <$> tokenize (intoLocated "0b0100.011") `shouldBe` Just (Tok.Number $ Binary "0100" $ Just "011")
    it "parses Operators" $ item . fst <$> tokenize (intoLocated "++>") `shouldBe` Just (Op "++>")
    it "parses Identifier" $ item . fst <$> tokenize (intoLocated "test") `shouldBe` Just (Ident "test")
    it "skips Comment and parses a token" $
      fst <$> tokenize (intoLocated "# Here is comment... \na") `shouldBe` Just (Ident "a" :@: Location 2 1)
    it "parses Parentheses as special symbol" $ do
      item . fst <$> tokenize (intoLocated "(") `shouldBe` Just LeftParenthese
      item . fst <$> tokenize (intoLocated ")") `shouldBe` Just RightParenthese
  describe "Parser" $ do
    it "parses basic expression factors" $ do
      fmap item . fst <$> parseText packageBlock "2" `shouldBe` Right [ValueExpr $ P.Num $ Decimal "2" Nothing]
      fmap item . fst <$> parseText packageBlock "a" `shouldBe` Right [ValueExpr $ Var "a"]
    it "parses infix expression at correct position" $ do
      fst <$> parseText packageBlock "2 + 3" `shouldBe`
        Right [ValueExpr (Infix (P.Num $ Decimal "2" Nothing) [("+", P.Num $ Decimal "3" Nothing) :@: Location 1 3]) :@: def]
      fmap item . fst <$> parseText packageBlock "a `shouldBe` b" `shouldBe`
        Right [ValueExpr $ Infix (Var "a") [("shouldBe", Var "b") :@: Location 1 4]]
    it "parses negating expression" $
      fmap item . fst <$> parseText packageBlock "-b" `shouldBe` Right [ValueExpr $ Neg $ Var "b"]
    it "parses applying" $ do
      fmap item . fst <$> parseText packageBlock "f x" `shouldBe` Right [ValueExpr $ Apply (Var "f") (Var "x")]
      fmap item . fst <$> parseText packageBlock "(+) 3" `shouldBe` Right [ValueExpr $ Apply (Var "+") (P.Num $ Decimal "3" Nothing)]
    it "parses tuple, expression or unit" $ do
      fmap item . fst <$> parseText packageBlock "(a, b)" `shouldBe` Right [ValueExpr $ Tuple [Var "a" :@: Location 1 2, Var "b" :@: Location 1 5]]
      fmap item . fst <$> parseText packageBlock "(c)" `shouldBe` Right [ValueExpr $ Var "c"]
      fmap item . fst <$> parseText packageBlock "()" `shouldBe` Right [ValueExpr Unit]
    it "parses lists" $ do
      fmap item . fst <$> parseText packageBlock "[a, b, c]" `shouldBe` Right [ValueExpr $ List [Var "a" :@: Location 1 2, Var "b" :@: Location 1 5, Var "c" :@: Location 1 8]]
      fmap item . fst <$> parseText packageBlock "[a..]" `shouldBe` Right [ValueExpr $ ArithmeticSeq (Var "a" :@: Location 1 2) Nothing Nothing]
      fmap item . fst <$> parseText packageBlock "[a, b..]" `shouldBe` Right [ValueExpr $ ArithmeticSeq (Var "a" :@: Location 1 2) (Just $ Var "b" :@: Location 1 5) Nothing]
    it "parses complex expression" $
      isRight $ runParser packageBlock (parseLayout $ tokenizeAll $ intoLocated "(fconv . (+ 2)) <$> [0,1..2] `shouldBe` [2, 4, 6]")
    it "parses basic patterns" $ do
      item . fst <$> (runParser pat $ tokenizeAll $ intoLocated "test") `shouldBe` Right (P.VarP "test")
      item . fst <$> (runParser pat $ tokenizeAll $ intoLocated "(test)") `shouldBe` Right (P.VarP "test")
      item . fst <$> (runParser pat $ tokenizeAll $ intoLocated "2") `shouldBe` Right (P.NumP $ Decimal "2" Nothing)
    it "parses complex patterns" $ do
      (isRight $ runParser pat $ tokenizeAll $ intoLocated "Cons 2 :@: p") `shouldBe` True
      (isRight $ runParser pat $ tokenizeAll $ intoLocated "(_ : t0@(TokParse2 a b) : ts)") `shouldBe` True
    it "parses let bindings" $ do
      (isRight $ parseText packageBlock "let c = 2") `shouldBe` True
    it "parses do block" $ do
      (const () <$> parseText packageBlock "let c = do { let d = 0; d * 3.0 }") `shouldBe` Right ()

parseText :: Parser a -> Text -> Either [Located Token] (a, [Located Token])
parseText p = runParser p . parseLayout . tokenizeAll . intoLocated