--Script para gerar o início de um arquivo .ppm
module RaytracerEtapa1_1113331018 where

--Creating a function to tranform pixel in string
pixelToString :: (Int, Int, Int) -> String
pixelToString (x, y, z) = (show x) ++ " " ++ (show y) ++ " " ++ (show z)  ++ "\n"

--Some contants to write a .ppm image
p3 = "P3\n"
comment = "# It's a .ppm imagem for a raytracer\n"

--How map works it receive a function and a list and aplly this function on each elemento of the list
--How foldr(reduce starting the right side of the list) works it receive a function, a start item and a list and join the elements in a String
create_text_to_ppm_file :: Int -> Int -> [(Int, Int, Int)] -> String
create_text_to_ppm_file _ _ [] = "Nao e possivel criar uma imagem sem pixels"
create_text_to_ppm_file width height pixels = (p3 ++ comment ++ (show width) ++ " " ++ (show height) ++ "\n" ++ "255\n" ++ (foldr (++) "" (map pixelToString pixels)))

-- Cria um um tipo Cor que é formado por uma tupla de inteiro e um outro tipo coordenada que é uma tupla de Double
-- e deriva da classe Show a forma de imprimir
data Vetor  = Pixel(Int, Int, Int)
            | Coordenada(Double, Double, Double)
            deriving (Show)