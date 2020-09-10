import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "freeProgram" $ do
    it "should return a valid User" $
      execAlgebra freeProgram `shouldReturn` User (UserId "123")
