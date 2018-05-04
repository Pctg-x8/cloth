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
      testFragment expr "2" `shouldBe` Right (P.Num $ Decimal "2" Nothing)
      testFragment expr "a" `shouldBe` Right (Var "a")
    it "parses infix expression at correct position" $ do
      fst <$> (runParser expr $ tokenizeAll $ intoLocated "2 + 3") `shouldBe`
        Right ((Infix (P.Num $ Decimal "2" Nothing) [("+", P.Num $ Decimal "3" Nothing) :@: Location 1 3]) :@: def)
      testFragment expr "a `shouldBe` b" `shouldBe` Right (Infix (Var "a") [("shouldBe", Var "b") :@: Location 1 4])
    it "parses negating expression" $
      testFragment expr "-b" `shouldBe` Right (Neg $ Var "b")
    it "parses applying" $ do
      testFragment expr "f x" `shouldBe` Right (Apply (Var "f") (Var "x"))
      testFragment expr "(+) 3" `shouldBe` Right (Apply (Var "+") (P.Num $ Decimal "3" Nothing))
    it "parses tuple, expression or unit" $ do
      testFragment expr "(a, b)" `shouldBe` Right (Tuple [Var "a" :@: Location 1 2, Var "b" :@: Location 1 5])
      testFragment expr "(c)" `shouldBe` Right (Var "c")
      testFragment expr "()" `shouldBe` Right Unit
    it "parses lists" $ do
      testFragment expr "[a, b, c]" `shouldBe` Right (List [Var "a" :@: Location 1 2, Var "b" :@: Location 1 5, Var "c" :@: Location 1 8])
      testFragment expr "[a..]" `shouldBe` Right (ArithmeticSeq (Var "a" :@: Location 1 2) Nothing Nothing)
      testFragment expr "[a, b..]" `shouldBe` Right (ArithmeticSeq (Var "a" :@: Location 1 2) (Just $ Var "b" :@: Location 1 5) Nothing)
    it "parses complex expression" $
      (isRight $ runParser expr $ tokenizeAll $ intoLocated "(fconv . (+ 2)) <$> [0,1..2] `shouldBe` [2, 4, 6]") `shouldBe` True
    it "parses basic patterns" $ do
      testFragment pat "test" `shouldBe` Right (P.VarP "test")
      testFragment pat "(test)" `shouldBe` Right (P.VarP "test")
      testFragment pat "2" `shouldBe` Right (P.NumP $ Decimal "2" Nothing)
    it "parses complex patterns" $ do
      (isRight $ runParser pat $ tokenizeAll $ intoLocated "Cons 2 :@: p") `shouldBe` True
      (isRight $ runParser pat $ tokenizeAll $ intoLocated "(_ : t0@(TokParse2 a b) : ts)") `shouldBe` True
    it "parses let bindings" $ do
      (const () <$> parseText packageBlock "main does let c = 2") `shouldBe` Right ()
    it "parses do block" $ do
      (const () <$> parseText packageBlock "main does let c = do { let d = 0; d * 3.0 }") `shouldBe` Right ()
    it "parses conditional expr" $
      (const () <$> parseText packageBlock "main = if f x == 2 then 0 else 2") `shouldBe` Right ()

testFragment :: Parser (Located a) -> Text -> Either [Located Token] a
testFragment p = fmap (item . fst) . runParser p . tokenizeAll . intoLocated
parseText :: Parser a -> Text -> Either [Located Token] (a, [Located Token])
parseText p = runParser p . parseLayout . tokenizeAll . intoLocated