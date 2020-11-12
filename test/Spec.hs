import AdventOfCode
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day 1, fuel requirement" $ do
    it "returns non negative fuel" $ do
      fuel 2 `shouldBe` 0
    it "returns 2 for 12" $ do
      fuel 12 `shouldBe` 2
    it "returns the same value for 14" $ do
      fuel 14 `shouldBe` fuel 12
    it "returns 4 for [12, 14]" $ do
      modulesFuelRequirement [12, 14] `shouldBe` 4

  describe "day 1, fuel including fuel requirement" $ do
    it "returns 2 for 14" $ do
      fuelIncFuel 14 `shouldBe` 2
    it "returns 966 for 1969" $ do
      fuelIncFuel 1969 `shouldBe` 966
    it "returns 50346 for 100756" $ do
      fuelIncFuel 100756 `shouldBe` 50346
