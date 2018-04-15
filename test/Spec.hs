{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Language.Cloth.Tokenizer

main :: IO ()
main = hspec $ do
  describe "Tokenizer" $ do
    it "advances line by LF" $ snd <$> tokenize (intoLocated "\n") `shouldBe` Just ("" :@: Location 2 1)
    it "leaves trailing correctly" $ snd <$> tokenize (intoLocated "3 ") `shouldBe` Just (" " :@: Location 1 2)
    it "parses Int" $ item . fst <$> tokenize (intoLocated "36 ") `shouldBe` Just (Int "36")
    it "parses Float" $ item . fst <$> tokenize (intoLocated "36.02") `shouldBe` Just (Float "36" "02")
    it "parses Hexadecimals" $ item . fst <$> tokenize (intoLocated "0x3a") `shouldBe` Just (IntH "3a")
    it "parses Octadecimals" $ item . fst <$> tokenize (intoLocated "0o7.5") `shouldBe` Just (FloatO "7" "5")
    it "parses Binary Literals" $ item . fst <$> tokenize (intoLocated "0b0100.011") `shouldBe` Just (FloatB "0100" "011")
    it "parses Operators" $ item . fst <$> tokenize (intoLocated "++>") `shouldBe` Just (Op "++>")
    it "parses Identifier" $ item . fst <$> tokenize (intoLocated "test") `shouldBe` Just (Ident "test")
    it "skips Comment and parses a token" $
      fst <$> tokenize (intoLocated "# Here is comment... \na") `shouldBe` Just (Ident "a" :@: Location 2 1)
