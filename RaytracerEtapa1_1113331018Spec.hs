import Test.Hspec
import RaytracerEtapa1_1113331018


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

  describe "a valid string to the .ppm image" $ do
    it "returns a valid string for a .ppm image" $ do
      (create_text_to_ppm_file 2 3 [(1, 2, 3)])  `shouldBe` "P3\n# It's a .ppm imagem for a raytracer\n2 3\n255\n1 2 3\n"
