import Test.Hspec
import HMM

main :: IO ()
main = hspec $ do
  describe "HMM" $ do
    it "prob1 evaluates trial1 properly" $ do
      prob1 exampleHMM trial1 `shouldBe` 0.00003951627542528
    it "prob1 evaluates trial2 properly" $ do
      prob1 exampleHMM trial2 `shouldBe` 0.000035757147508736
