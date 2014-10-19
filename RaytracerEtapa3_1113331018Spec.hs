import Test.Hspec
import RaytracerEtapa3_1113331018


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

  describe "the aritmethic operations for Coordenada" $ do
    it "returns vetorial sum of two vectors" $ do
      (Coordenada(1, 2, 3) $+ Coordenada(1, 2, 3)) `shouldBe` Coordenada(2, 4, 6)

    it "returns a vetorial minus of two vectors" $ do
      (Coordenada(1, 2, 3) $- Coordenada(1, 2, 3)) `shouldBe` Coordenada(0, 0, 0)

    it "returns each elements times escalar value" $ do
      (Coordenada(1, 2, 3) $* 3) `shouldBe` Coordenada(3, 6, 9)

    it "returns each elements divided by escalar value" $ do
      (Coordenada(3, 3, 3) $/ 3) `shouldBe` Coordenada(1, 1, 1)

    it "returns a sum of each element of the vector times its correpondent in the second one" $ do
      (Coordenada(1, 2, 3) $. Coordenada(1, 2, 3)) `shouldBe` 14

  describe "the aritmethic operations for Pixel" $ do
    it "returns vetorial sum of two vectors" $ do
      (Pixel(1, 2, 3) $+ Pixel(1, 2, 3)) `shouldBe` Pixel(2, 4, 6)

    it "returns a vetorial minus of two vectors" $ do
      (Pixel(1, 2, 3) $- Pixel(1, 2, 3)) `shouldBe` Pixel(0, 0, 0)

    it "returns each elements times escalar value" $ do
      (Pixel(1, 2, 3) $* 3) `shouldBe` Pixel(3, 6, 9)

    it "returns each elements divided by escalar value" $ do
      (Pixel(3, 3, 3) $/ 3) `shouldBe` Pixel(1, 1, 1)

    it "returns a sum of each element of the vector times its correpondent in the second one" $ do
      (Pixel(1, 2, 3) $. Pixel(1, 2, 3)) `shouldBe` 14