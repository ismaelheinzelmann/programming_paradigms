module ComparisonSudoku where
import Data.List ( nub )
import Data.Maybe
import System.FilePath (isValid)
import Distribution.Simple.Command (OptDescr(BoolOpt))
import Data.Map (valid)

type Board = [[Int]]
data Comparison = Less | Greater | None deriving (Eq, Show)
type ComparisonRows = [[Comparison]]
type ComparisonColumns = [[Comparison]]

-- Sudoku de Comparação é um sudoku com comparações entre os números
-- Linhas e colunas de comparação são matrizes de comparações
-- Cada linha e coluna de comparação terá 6 elementos, e terão 9 linhas/colunas de comparação
-- Cada elemento é uma comparação entre dois números
-- Cada comparação é um Less ou um Greater

--tested
getComparatorUp :: ComparisonColumns -> Int -> Int -> Comparison
getComparatorUp comparisionColumns row col
    | row `mod` 3 == 0 = None
    | otherwise = comparisionColumns !! col !! (row-1)
--tested
getComparatorDown :: ComparisonColumns -> Int -> Int -> Comparison
getComparatorDown comparisionColumns row col
    | row `mod` 3 == 2 = None
    | otherwise = comparisionColumns !! col !! row

--tested
getComparatorLeft :: ComparisonRows -> Int -> Int -> Comparison
getComparatorLeft comparisionRows row col
    | col `mod` 3 == 0 = None
    | otherwise = comparisionRows !! row !! (col-1)

--tested
getComparatorRight :: ComparisonRows -> Int -> Int -> Comparison
getComparatorRight comparisionRows row col
    | col `mod` 3 == 2 = None
    | otherwise = comparisionRows !! row !! col

getCell :: Board -> Int -> Int -> Int
getCell board row col
    | row < 0 = 0
    | row > 8 = 0
    | col < 0 = 0
    | col > 8 = 0
    | otherwise = board !! row !! col


-- Linha, Coluna, Valor
validRow :: Board -> ComparisonRows -> Int -> Bool
validRow board comparisionRows row = length (nub (getRow board row)) == length (getRow board row)

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

--verify if have any repeated values in the column
validColumn :: Board -> ComparisonColumns -> Int -> Bool
validColumn board comparisionColumns col = length (nub (getColumn board col)) == length (getColumn board col)

validBlock :: Board -> Int -> Int -> Bool
validBlock board row col =
    let block = getBlock board row col
    in length (nub block) == length block


validBoard :: Board -> ComparisonRows -> ComparisonColumns -> Bool
validBoard board comparisionRows comparisionColumns =
    let validRows = all (validRow board comparisionRows) [0..8]
        validColumns = all (validColumn board comparisionColumns) [0..8]
        validBlocks = all (\row -> all (validBlock board row) [0,3,6]) [0,3,6]
        validPlaces = all (\row -> all (validCellComparisions board comparisionRows comparisionColumns row) [0..8]) [0..8]
    in validRows && validColumns && validBlocks && validPlaces

replace :: [[Int]] -> Int -> Int -> Int -> [[Int]]
replace block row col value =
    let (xs, y:ys) = splitAt row block
        (zs, _:ws) = splitAt col y
    in xs ++ [zs ++ [value] ++ ws] ++ ys

--Verify if the board full
isFull :: Board -> Bool
isFull = all (notElem 0)

--Find first empty position, position is empty if value is Nothing
firstEmpty :: Board -> (Int, Int)
firstEmpty board = head [(row, col) | row <- [0..8], col <- [0..8], board !! row !! col == 0]

--Print board
printBoard :: Board -> IO ()
printBoard = mapM_ print


--tested
getBlock :: Board -> Int -> Int -> [Int]
getBlock board row col =
    let blockRow = row `div` 3
        blockCol = col `div` 3
        startRow = blockRow * 3
        startCol = blockCol * 3
    in [board !! row !! col | row <- [startRow..startRow + 2], col <- [startCol..startCol + 2]]

getRow :: Board -> Int -> [Int]
getRow board row = board !! row

getColumn :: Board -> Int -> [Int]
getColumn board col = map (!! col) board

backtrack :: Board -> ComparisonRows -> ComparisonColumns -> Maybe Board
backtrack board comparisionRows comparisionColumns
    | isFull board = Just board
    | otherwise =
        let (row, col) = firstEmpty board
            possibleValues = [1..9]
            validValues = filter (\value -> validCellComparisions board comparisionRows comparisionColumns row col) possibleValues
            solutions = map (\value -> backtrack (replace board row col value) comparisionRows comparisionColumns) validValues
        in listToMaybe (catMaybes solutions)