
module ComparisonSudoku where
import Data.List ( nub, (\\) )
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Debug.Trace

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
validRow :: Board -> Int -> Bool
validRow board rowNumber = do
    --filter row to remove 0s
    let row = filter (/= 0) (getRow board rowNumber)
    length (nub row) == length row

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
validColumn :: Board -> Int -> Bool
validColumn board col = do
    let column = filter (/= 0) (getColumn board col)
    length (nub column) == length column

validBlock :: Board -> Int -> Int -> Bool
validBlock board row col =
    let block = getBlock board row col
        blockWithoutZeros = filter (/= 0) block
    in length (nub blockWithoutZeros) == length blockWithoutZeros


validBoard :: Board -> ComparisonRows -> ComparisonColumns -> Bool
validBoard board comparisionRows comparisionColumns =
    let validRows = all (validRow board) [0..8]
        validColumns = all (validColumn board) [0..8]
        validBlocks = all (\row -> all (validBlock board row) [0,3,6]) [0,3,6]
        validPlaces = all (\row -> all (validCellComparisions board comparisionRows comparisionColumns row) [0..8]) [0..8]
    in validRows && validColumns && validBlocks && validPlaces

--Verify if the value is valid to be placed in the next empty cell on board
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

replace :: [[Int]] -> Int -> Int -> Int -> [[Int]]
replace block row col value =
    let (xs, y:ys) = splitAt row block
        (zs, _:ws) = splitAt col y
    in xs ++ [zs ++ [value] ++ ws] ++ ys

isFull :: [[Int]] -> Bool
isFull = not . any (elem 0)

--find first row col containing 0 in board, return Nothing if not found
firstEmpty :: Board -> Maybe (Int, Int)
firstEmpty board = listToMaybe [(row, col) | row <- [0..8], col <- [0..8], board !! row !! col == 0]

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

insertFirstEmpty :: Board -> Int -> Board
insertFirstEmpty board value = do
    let empty = firstEmpty board
    case empty of
        Nothing -> board
        Just (row, col) -> replace board row col value

solveSudokuBacktracking :: Board -> ComparisonRows -> ComparisonColumns -> Maybe Board
solveSudokuBacktracking board comparisionRows comparisionColumns =
    if isFull board then do
        traceM $ "Full" ++ show board
        if validBoard board comparisionRows comparisionColumns
            then do
                Just board
            else Nothing
    else do
        traceM $ "Not Full: " ++ show board
        let validPlacements = filter (\x -> validInsert board comparisionRows comparisionColumns x) [1..9]
        if null validPlacements then Nothing else listToMaybe $ mapMaybe (\value -> solveSudokuBacktracking (insertFirstEmpty board value) comparisionRows comparisionColumns) validPlacements
