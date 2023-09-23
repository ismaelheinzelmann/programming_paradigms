module Solver where
import Data.List 
-- Definindo a matriz e seus elementos 
type Grid             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Value            = Char

-----------------
-- Definições basicas de um sudoku (tamanho da caixa, valor da casas, vazio ou não e se tem apenas uma opção)
boxsize :: Int
boxsize               =  3

values :: [Value]
values                =  ['1'..'9']

empty :: Value -> Bool
empty                 =  (== '0')

single :: [a] -> Bool
single [_]            =  True
single _              =  False

-------------

-- Matriz de Teste
{-
sudoku                  :: Grid
sudoku                  =   ["200001038",
                            "000000005",
                            "070006000",
                            "000000013",
                            "098100257",
                            "310000800",
                            "900800020",
                            "050069784",
                            "400250000"]

-- Grid vazio para poder preencher

vazia                 :: Grid
vazia                 =  replicate n (replicate n '0')
                          where n = boxsize ^ 2
-}

----------------------------------
-- Separando linhas, colunas e caixas

-- Recebe a matriz e retorna a linha
linhas :: Matrix a -> [Row a] 
linhas                  =  id -- A linha é igual ao id da linha

-- Recebe a matriz e retorna a coluna
colunas :: Matrix a -> [Row a] 
colunas                  =  transpose -- Função modolo List para transpor a coluna a partir do input
 
{-
   Usando o operador "." para ligar o output de cada uma das funções para não precisar chamar dentro de cada uma delas
-}

-- Recebe a matriz e devolve cada caixa(dependendo do tamanho dela(3x3 para 9x9, 3x2 para 6x6 e 2x2 para 4x4))
caixa :: Matrix a -> [Row a]
caixa                  =  desempacotar . map colunas . empacotar
                          where
                             empacotar   = separar . map separar
                             separar  = divide boxsize
                             desempacotar = map concat . concat

-- Divide a partir do tamanho da caixa(n)
divide :: Int -> [a] -> [[a]]
divide n []             =  []
divide n xs             =  take n xs : divide n (drop n xs)


---------------------------------------------------------------

-- Verifica se não tem duplicadatas em todas as linhas, colunas e caixas a partir do recebimento da matriz
valid :: Grid -> Bool
valid g               =  all nodups (linhas g) &&
                          all nodups (colunas g) &&
                          all nodups (caixa g)

-- Verifica duplicatas
nodups                :: Eq a => [a] -> Bool
nodups []             =  True
nodups (x:xs)         =  not (elem x xs) && nodups xs

---------------------------------------------------------------
{-
=====================================================================================================
   A partir daqui é otimização do código inicial para rodar, pois no meu notebook não estava rodando 
=====================================================================================================
-}

-- Definindo as opções possiveis
type Choices          =  [Value]

-- Mapeia cada escolha possivel para cada casa 
choices               :: Grid -> Matrix Choices
choices               =  map (map choice)
                          where
                             choice v = if empty v then values else [v]


-- Lista com todas as opções possiveis 
cp                    :: [[a]] -> [[a]]
cp []                 =  [[]]
cp (xs:xss)           =  [y:ys | y <- xs, ys <- cp xss]


-- Lista todas as opções da matriz
collapse              :: Matrix [a] -> [Matrix a]
collapse              =  cp . map cp

-- Reduzindo as escolhas posiveis 
prune                 :: Matrix Choices -> Matrix Choices
prune                 =  pruneBy caixa . pruneBy colunas . pruneBy linhas
                          where pruneBy f = f . map reduce . f

reduce                :: Row Choices -> Row Choices
reduce xss            =  [xs `minus` singles | xs <- xss]
                          where singles = concat (filter single xss)

minus                 :: Choices -> Choices -> Choices
xs `minus` ys         =  if single xs then xs else xs \\ ys

-- Encontrar um limite fixo onde não faz mais sentido tentar reduzir 
fix                   :: Eq a => (a -> a) -> a -> a
fix f x               =  if x == x' then x else fix f x'
                          where x' = f x

---------------------------------------------------------------

solve                :: Grid -> [Grid]
solve                =  filter valid . collapse . fix prune . choices