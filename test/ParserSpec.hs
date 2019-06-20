module ParserSpec where
import Test.Hspec

import Parser


parserSpec = do
  describe "readExpr" $ do
    it "should parse a simple lambda expression" $ do {
      readExpr "x y"
      `shouldBe`
      List [Variable "x",Variable "y"]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "x y "
      `shouldBe`
      List [Variable "x",Variable "y"]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr " I:=a b  \\x.x"
      `shouldBe`
      List [Def (Id "I") (List [Variable "a",Variable "b",Lambda (Variable "x") (List [Variable "x"])])]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr " I:=(a b)  \\x.x"
      `shouldBe`
      List [Def (Id "I") (List [Brackets (List [Variable "a",Variable "b"]),Lambda (Variable "x") (List [Variable "x"])])]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr " (I:=a b)  \\x.x"
      `shouldBe`
      List [Brackets (List [Def (Id "I") (List [Variable "a",Variable "b"])]),Lambda (Variable "x") (List [Variable "x"])]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "\\x.x"
      `shouldBe`
      List [Lambda (Variable "x") (List [Variable "x"])]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "\\x.x  \\z.\\k.z"
      `shouldBe`
      List [Lambda (Variable "x") (List [Variable "x",Lambda (Variable "z") (List [Lambda (Variable "k") (List [Variable "z"])])])]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "(\\x.x k)  \\z.\\k.z"
      `shouldBe`
      List [Brackets (List [Lambda (Variable "x") (List [Variable "x",Variable "k"])]),Lambda (Variable "z") (List [Lambda (Variable "k") (List [Variable "z"])])]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "((\\x.x k) k)  \\z.\\k.z"
      `shouldBe`
      List [Brackets (List [Brackets (List [Lambda (Variable "x") (List [Variable "x",Variable "k"])]),Variable "k"]),Lambda (Variable "z") (List [Lambda (Variable "k") (List [Variable "z"])])]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "(\\x.x) y"
      `shouldBe`
      List [Brackets $ List [(Lambda (Variable "x") (List [Variable "x"]))], Variable "y"]
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "T:=(\\x.\\y.x) I:=(\\x.x) I T"
      `shouldBe`
      List [Def (Id "T") (List [Brackets (List [Lambda (Variable "x") (List [Lambda (Variable "y") (List [Variable "x"])])]),Def (Id "I") (List [Brackets (List [Lambda (Variable "x") (List [Variable "x"])]),Id "I",Id "T"])])]
    }
