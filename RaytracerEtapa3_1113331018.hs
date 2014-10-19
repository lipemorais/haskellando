--Script para gerar o início de um arquivo .ppm


module RaytracerEtapa3_1113331018 where

--Cria um um tipo Cor que é formado por uma tupla de inteiro e um outro tipo coordenada que é uma tupla de Double
--e deriva da classe Show a forma de imprimir

--O data nada mais é que um guarda chuva de construtores de objetos no meu caso tenho dois tipos de construtores um para Pixel outro para Coordenada ambos são Vetor3D

data Vetor3D  = Pixel(Int, Int, Int)
            | Coordenada(Double, Double, Double)
            deriving (Show, Eq)

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


--Soma vetorial: (x1, y1, z1) + (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
($+) :: Vetor3D -> Vetor3D -> Vetor3D
Coordenada(x1, y1, z1) $+ Coordenada(x2, y2, z2) = Coordenada(x1 + x2, y1 + y2, z1 + z2)
Pixel(x1, y1, z1) $+ Pixel(x2, y2, z2) = Pixel(x1 + x2, y1 + y2, z1 + z2)

--Subtração vetorial: (x1, y1, z1) − (x2, y2, z2) = (x1 − x2, y1 − y2, z1 − z2)
($-) :: Vetor3D -> Vetor3D -> Vetor3D
Coordenada(x1, y1, z1) $- Coordenada(x2, y2, z2) = Coordenada(x1 - x2, y1 - y2, z1 - z2)
Pixel(x1, y1, z1) $- Pixel(x2, y2, z2) = Pixel(x1 - x2, y1 - y2, z1 - z2)

--Produto escalar: (x1, y1, z1).(x2, y2, z2) = x1x2 + y1y2 + z1z2
($.) :: Vetor3D -> Vetor3D -> Double
Coordenada(x1, y1, z1) $. Coordenada(x2, y2, z2) = ((x1 * x2) + (y1 * y2) + (z1 * z2))
Pixel(x1, y1, z1) $. Pixel(x2, y2, z2) = fromIntegral ((x1 * x2) + (y1 * y2) + (z1 * z2))

--Multiplicação por escalar: a(x1, y1, z1) = (ax1, ay1, az1)
($*) :: Vetor3D -> Double -> Vetor3D
Coordenada(x1, y1, z1) $* escalar = Coordenada(x1 * escalar, y1 * escalar, z1 * escalar)
Pixel(x1, y1, z1) $* escalar = Pixel(x1 * (floor escalar), y1 * (floor escalar), z1 * (floor escalar))

--Divisão por escalar: (x1, y1, z1)/a = (x1/a, y1/a, z1/a)
($/) :: Vetor3D -> Double -> Vetor3D
Coordenada(x1, y1, z1) $/ escalar = Coordenada(x1 / escalar, y1 / escalar, z1 / escalar)
Pixel(x1, y1, z1) $/ escalar = Pixel(floor((fromIntegral x1) / escalar), floor(fromIntegral y1 / escalar), floor(fromIntegral z1 / escalar))

--OBS.:
--Cores que para mim são pixels

--TODO



--Usar a dica que o Rafael deu no e-mail para melhorar os testes:
--Que tal experimentar um pouco com a função property também?
--Por exemplo, a função pixelToString sempre gera dois espaços em branco, a gente poderia escrever o seguinte:

--    it "should always have two empty spaces" $
--        property $ \x y z -> (length $ filter (==' ') $ pixelToString (x, y, z)) == 2

--    Não testei para ver se funciona :P, mas acho que é isso.
--    Quais outras invariantes a gente consegue achar no código?
--    A parte com operações vetoriais pode ter umas interessantes.
--    Por exemplo, a expressão a seguir deve ser verdadeira
--    para todo x diferente de 0:
--    v $* x $/ x == v.
