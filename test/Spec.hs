{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Language.Cloth.Tokenizer as Tok
import Language.Cloth.Tokenizer
import qualified Language.Cloth.Parser as P
import Language.Cloth.Parser
import Data.Default (def)

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
      item . fst <$> runParser expr (tokenizeAll (intoLocated "2")) `shouldBe` Right (P.Number $ Decimal "2" Nothing)
      item . fst <$> runParser expr (tokenizeAll (intoLocated "a")) `shouldBe` Right (Var "a")
    it "parses infix expression at correct position" $ do
      fst <$> runParser expr (tokenizeAll (intoLocated "2 + 3")) `shouldBe`
        Right ((Infix (P.Number $ Decimal "2" Nothing) "+" (P.Number $ Decimal "3" Nothing)) :@: def)
      item . fst <$> runParser expr (tokenizeAll (intoLocated "a `shouldBe` b")) `shouldBe`
        Right (Infix (Var "a") "shouldBe" (Var "b"))
    it "parses negating expression" $ do
      item . fst <$> runParser expr (tokenizeAll (intoLocated "-b")) `shouldBe` Right (Neg $ Var "b")
    it "parses applying" $ do
      item . fst <$> runParser expr (tokenizeAll (intoLocated "f x")) `shouldBe` Right (Apply (Var "f") (Var "x"))
      item . fst <$> runParser expr (tokenizeAll $ intoLocated "(+) 3") `shouldBe` Right (Apply (Var "+") (P.Number $ Decimal "3" Nothing))
