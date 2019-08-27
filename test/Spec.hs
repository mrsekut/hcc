import           Text.Parsec
import           Test.Hspec
import           Parser


parseSucc :: String -> Stmt
parseSucc input = case parse program "Example" input of
  Left{}    -> error "Parse failure"
  Right str -> str


-- parseFail :: String -> String
-- parseFail input = case parse program "Example" input of
--   Left{}  -> "no parse"
--   Right _ -> error "Parsed but shouldn't"


main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "parse" $ do
      parseSucc "3+3;" `shouldBe` Stmt [Add (Nat 3) (Nat 3)]
      parseSucc "3 + 3;" `shouldBe` Stmt [Add (Nat 3) (Nat 3)]
