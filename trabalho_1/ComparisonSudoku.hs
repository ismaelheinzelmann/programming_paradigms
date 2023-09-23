module ComparisonSudoku where

import Data.List (nub, (\\))
import Data.Maybe
import Debug.Trace

type Board = [[Int]]

data Comparison = Less | Greater | None deriving (Eq, Show)

type ComparisonRows = [[Comparison]]

type ComparisonColumns = [[Comparison]]

-- Get do comparador de cima dado uma cordenada i j
getComparatorUp :: ComparisonColumns -> Int -> Int -> Comparison
getComparatorUp comparisionColumns row col
  | row `mod` 3 == 0 = None
  | otherwise = comparisionColumns !! col !! (row - 1)

-- Get do comparador de baixo dado uma cordenada i j
getComparatorDown :: ComparisonColumns -> Int -> Int -> Comparison
getComparatorDown comparisionColumns row col
  | row `mod` 3 == 2 = None
  | otherwise = comparisionColumns !! col !! row

-- Get do comparador da esquerda dado uma cordenada i j
getComparatorLeft :: ComparisonRows -> Int -> Int -> Comparison
getComparatorLeft comparisionRows row col
  | col `mod` 3 == 0 = None
  | otherwise = comparisionRows !! row !! (col - 1)

-- Get do comparador da direita dado uma cordenada i j
getComparatorRight :: ComparisonRows -> Int -> Int -> Comparison
getComparatorRight comparisionRows row col
  | col `mod` 3 == 2 = None
  | otherwise = comparisionRows !! row !! col

-- Retorna o valor da celula na linha e coluna
getCell :: Board -> Int -> Int -> Int
getCell board row col
  | row < 0 = 0
  | row > 8 = 0
  | col < 0 = 0
  | col > 8 = 0
  | otherwise = board !! row !! col

-- Verifica se a linha é valida (não tem valores repetidos até o momento)
validRow :: Board -> Int -> Bool
validRow board rowNumber = do
  -- filter row to remove 0s
  let row = filter (/= 0) (getRow board rowNumber)
  length (nub row) == length row

-- Verifica se a célula é valida (se os comparadores dos vizinhos são satisfeitos)
validCellComparisions :: Board -> ComparisonRows -> ComparisonColumns -> Int -> Int -> Bool
validCellComparisions board comparisionRows comparisionColumns row col =
  let comparatorUp = getComparatorUp comparisionColumns row col
      comparatorDown = getComparatorDown comparisionColumns row col
      comparatorLeft = getComparatorLeft comparisionRows row col
      comparatorRight = getComparatorRight comparisionRows row col
      left = getCell board row (col - 1)
      right = getCell board row (col + 1)
      up = getCell board (row - 1) col
      down = getCell board (row + 1) col
      value = board !! row !! col
      validUp = case comparatorUp of
        None -> True
        Less -> case up of
          0 -> True
          otherwise -> up < value
        Greater -> case up of
          0 -> True
          otherwise -> up > value
      validDown = case comparatorDown of
        None -> True
        Less -> case down of
          0 -> True
          otherwise -> down > value
        Greater -> case down of
          0 -> True
          otherwise -> down < value
      validLeft = case comparatorLeft of
        None -> True
        Less -> case left of
          0 -> True
          otherwise -> left < value
        Greater -> case left of
          0 -> True
          otherwise -> left > value
      validRight = case comparatorRight of
        None -> True
        Less -> case right of
          0 -> True
          otherwise -> right > value
        Greater -> case right of
          0 -> True
          otherwise -> right < value
   in validUp && validDown && validLeft && validRight

-- Verifica se a coluna é valida (não tem valores repetidos até o momento)
validColumn :: Board -> Int -> Bool
validColumn board col = do
  let column = filter (/= 0) (getColumn board col)
  length (nub column) == length column

-- Verifica se o bloco é valido (não tem valores repetidos até o momento)
validBlock :: Board -> Int -> Int -> Bool
validBlock board row col =
  let block = getBlock board row col
      blockWithoutZeros = filter (/= 0) block
   in length (nub blockWithoutZeros) == length blockWithoutZeros

-- Verifica se o board é valido (colunas válidas, linhas válidas, blocos válidos e todas as células respeitam os comparadores)
validBoard :: Board -> ComparisonRows -> ComparisonColumns -> Bool
validBoard board comparisionRows comparisionColumns =
  let validRows = all (validRow board) [0 .. 8]
      validColumns = all (validColumn board) [0 .. 8]
      validBlocks = all (\row -> all (validBlock board row) [0, 3, 6]) [0, 3, 6]
      validPlaces = all (\row -> all (validCellComparisions board comparisionRows comparisionColumns row) [0 .. 8]) [0 .. 8]
   in validRows && validColumns && validBlocks && validPlaces

-- Verifica se a inserção de um valor na linha e coluna é valida
validInsert :: Board -> ComparisonRows -> ComparisonColumns -> Int -> Bool
validInsert board comparisionRows comparisionColumns value =
  let nextEmpty = firstEmpty board
   in case nextEmpty of
        Nothing -> validBoard board comparisionRows comparisionColumns
        Just (row, col) -> do
          let newBoard = insertFirstEmpty board value
              validComparisions = validCellComparisions (insertFirstEmpty board value) comparisionRows comparisionColumns row col
              validR = validRow newBoard row
              validC = validColumn newBoard col
              validB = validBlock newBoard row col
          validComparisions && validR && validC && validB

-- Altera o valor de uma celula na linha e coluna
replace :: [[Int]] -> Int -> Int -> Int -> [[Int]]
replace block row col value =
  let (xs, y : ys) = splitAt row block
      (zs, _ : ws) = splitAt col y
   in xs ++ [zs ++ [value] ++ ws] ++ ys

-- Verifica se o board está cheio
isFull :: [[Int]] -> Bool
isFull = not . any (elem 0)

-- Retorna a próxima cordenada válida para inserção
firstEmpty :: Board -> Maybe (Int, Int)
firstEmpty board = listToMaybe [(row, col) | row <- [0 .. 8], col <- [0 .. 8], board !! row !! col == 0]

-- Print board
printBoard :: Board -> IO ()
printBoard = mapM_ print

-- Retorna uma lista com o bloco da cordenada
getBlock :: Board -> Int -> Int -> [Int]
getBlock board row col =
  let blockRow = row `div` 3
      blockCol = col `div` 3
      startRow = blockRow * 3
      startCol = blockCol * 3
   in [board !! row !! col | row <- [startRow .. startRow + 2], col <- [startCol .. startCol + 2]]

-- Retorna a  n linha
getRow :: Board -> Int -> [Int]
getRow board row = board !! row

-- Retorna a n coluna
getColumn :: Board -> Int -> [Int]
getColumn board col = map (!! col) board

-- Insere o valor no primeiro espaço vazio
insertFirstEmpty :: Board -> Int -> Board
insertFirstEmpty board value = do
  let empty = firstEmpty board
  case empty of
    Nothing -> board
    Just (row, col) -> replace board row col value

-- Utiliza recursão para resolver o sudoku
solveSudokuRecursive :: Board -> ComparisonRows -> ComparisonColumns -> Maybe Board
solveSudokuRecursive board comparisionRows comparisionColumns =
  if isFull board
    then do
      if validBoard board comparisionRows comparisionColumns
        then do
          Just board
        else Nothing
    else do
      -- debug
      -- traceM $ "Not Full: " ++ show board
      let validPlacements = filter (\x -> validInsert board comparisionRows comparisionColumns x) [1 .. 9]
      if null validPlacements then Nothing else listToMaybe $ mapMaybe (\value -> solveSudokuRecursive (insertFirstEmpty board value) comparisionRows comparisionColumns) validPlacements
