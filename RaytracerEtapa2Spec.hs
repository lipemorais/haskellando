import Test.Hspec
import RaytracerEtapa2


main :: IO ()
main = hspec $ do
  describe "Setup of .ppm image" $ do
    it "returns a string with x, y and z" $ do
      pixelToString (1, 2, 3) `shouldBe` "1 2 3\n"

    it "returns the value of p3" $ do
      p3 `shouldBe` "P3\n"

    it "return a line of comment" $ do
      (head comment) `shouldBe` '#'

    it "returns \\n as the last char of a String" $ do
      (last p3) `shouldBe` '\n'
      (last comment) `shouldBe` '\n'

  describe "a invalid string to the .ppm file" $ do
    it "returns a error message" $ do
      (create_text_to_ppm_file 2 3 []) `shouldBe` "Nao e possivel criar uma imagem sem pixels"

  describe "the creation of a string to the .ppm image" $ do
    it "returns a valid string for the file" $ do
      (create_text_to_ppm_file 2 3 [(1, 2, 3)])  `shouldBe` "P3\n# It's a .ppm imagem for a raytracer\n2 3\n255\n1 2 3\n"

  describe "the aritmethic operations" $ do
    it "returns vetorial sum of two vectors" $ do
      ((1, 2, 3) $+ (1, 2, 3)) `shouldBe` (2, 4, 6)

    it "returns a vetorial minus of two vectors" $ do
      ((1, 2, 3) $- (1, 2, 3)) `shouldBe` (0, 0, 0)

    it "returns each elements times escalar value" $ do
      ((1, 2, 3) $* 3) `shouldBe` (3, 6, 9)

    it "returns each elements divided by escalar value" $ do
      ((3, 3, 3) $/ 3) `shouldBe` (1, 1, 1)

    it "returns a sum of each element of the vector times its correpondent in the second one" $ do
      ((1, 2, 3) $. (1, 2, 3)) `shouldBe` 14