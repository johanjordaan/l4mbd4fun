module ParserSpec where
import Test.Hspec

import L4mbd4Val
import Parser


parserSpec = do
  describe "readExpr" $ do
    it "should parse a simple lambda expression" $ do {
      readExpr "x y"
      `shouldBe`
      (Right $ List [Variable "x",Variable "y"])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "x y "
      `shouldBe`
      (Right $ List [Variable "x",Variable "y"])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr " I:=a b  \\x.x"
      `shouldBe`
      (Right $ List [Def "I" (List [Variable "a",Variable "b",Lambda "x" (List [Variable "x"])])])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr " I:=(a b)  \\x.x"
      `shouldBe`
      (Right $ List [Def "I" (List [List [Variable "a",Variable "b"],Lambda "x" (List [Variable "x"])])])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr " (I:=a b)  \\x.x"
      `shouldBe`
      (Right $ List [List [Def "I" (List [Variable "a",Variable "b"])],Lambda "x" (List [Variable "x"])])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "\\x.x"
      `shouldBe`
      (Right $ List [Lambda "x" (List [Variable "x"])])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "\\x.x  \\z.\\k.z"
      `shouldBe`
      (Right $ List [Lambda "x" (List [Variable "x",Lambda "z" (List [Lambda "k" (List [Variable "z"])])])])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "(\\x.x k)  \\z.\\k.z"
      `shouldBe`
      (Right $ List [List [Lambda "x" (List [Variable "x",Variable "k"])],Lambda "z" (List [Lambda "k" (List [Variable "z"])])])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "((\\x.x k) k)  \\z.\\k.z"
      `shouldBe`
      (Right $ List [List [List [Lambda "x" (List [Variable "x",Variable "k"])],Variable "k"],Lambda "z" (List [Lambda "k" (List [Variable "z"])])])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "(\\x.x) y"
      `shouldBe`
      (Right $ List [List [(Lambda "x" (List [Variable "x"]))], Variable "y"])
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "T:=(\\x.\\y.x) I:=(\\x.x) I T"
      `shouldBe`
      (Right $ List [Def "T" (List [List [Lambda "x" (List [Lambda "y" (List [Variable "x"])])],Def "I" (List [List [Lambda "x" (List [Variable "x"])],Id "I",Id "T"])])])
    }
