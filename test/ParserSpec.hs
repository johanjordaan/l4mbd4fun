module ParserSpec where
import Test.Hspec

import Parser

parserSpec = do
  describe "actionDef" $ do
    it "should parse a simple lambda expression" $ do {
      readExpr "\\z.\\k.j"
      `shouldBe`
      "Found value: Lambda (Variable \"z\") (Lambda (Variable \"k\") (Variable \"j\"))"
    }
    it "should parse a simple lambda expression" $ do {
      readExpr "\\z.\\k.j x"
      `shouldBe`
      "Found value: Lambda (Variable \"z\") (Lambda (Variable \"k\") (Variable \"j\"))"
    }
    it "should parse a simple lambda 'program'" $ do {
      readExpr "I:=\\x.x?\\z.\\k.j x"
      `shouldBe`
      "Found value: Program (List [Def (Id \"I\") (Lambda (Variable \"x\") (Variable \"x\"))]) (Lambda (Variable \"z\") (Lambda (Variable \"k\") (Variable \"j\")))"
    }
