module ComparisonSudoku where
import Data.List ( nub )
import Data.Maybe
import System.FilePath (isValid)

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
    | otherwise = comparisionColumns !! (row - 1) !! col
--tested
getComparatorDown :: ComparisonColumns -> Int -> Int -> Comparison
getComparatorDown comparisionColumns row col
    | row `mod` 3 == 2 = None
    | otherwise = comparisionColumns !! row !! col

--tested
getComparatorLeft :: ComparisonRows -> Int -> Int -> Comparison
getComparatorLeft comparisionRows row col
    | col `mod` 3 == 0 = None
    | otherwise = comparisionRows !! row !! (col - 1)

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
validRow :: Board -> ComparisonRows -> Int -> Int -> Bool
validRow board comparisionRows row col =
    let comparatorLeft = getComparatorLeft comparisionRows row col
        comparatorRight = getComparatorRight comparisionRows row col
        left = board !! row !! (col - 1)
        right = board !! row !! (col + 1)
        value = getCell board row col
        
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
        validLine = length (nub (board !! row)) == length (board !! row)
    in validLeft && validRight && validLine

validColumn :: Board -> ComparisonColumns -> Int -> Int -> Bool
validColumn board comparisionColumns row col =
    let comparatorUp = getComparatorUp comparisionColumns row col
        comparatorDown = getComparatorDown comparisionColumns row col
        up = board !! (row - 1) !! col
        down = board !! (row + 1) !! col
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
        validColumn = length (nub (map (!! col) board)) == length (map (!! col) board)
    in validUp && validDown && validColumn

validBlock :: Board -> Int -> Int -> Bool
validBlock board row col =
    let block = concat (take 3 (drop (row `div` 3 * 3) board))
        validBlock = length (nub block) == length block
    in validBlock

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

-- --Print board
-- printBoard :: Board -> IO ()
-- printBoard = mapM_ printRow
--     where
--         printRow :: [Maybe Int] -> IO ()
--         printRow row = putStrLn (concatMap showCell row ++ "\n")
--         showCell :: Maybe Int -> String
--         showCell Nothing = " "
--         showCell (Just n) = show n

--Apply validLine, validColumn and validBlock to the board
validBoard :: Board -> Bool
validBoard board =
    let validLine = all (\row -> length (nub row) == length row) board
        validColumn = all ((\col -> length (nub col) == length col) . (\col -> map (!! col) board)) [0..8]
        validBlock = all ((\block -> length (nub block) == length block) . (\block -> concat (take 3 (drop (block `div` 3 * 3) board)))) [0..8]
    in validLine && validColumn && validBlock

-- backtrack :: Board -> ComparisonRows -> ComparisonColumns -> Maybe Board
-- backtrack board comparisionRows comparisionColumns
--     | isFull board = Just board
--     | otherwise =
--         let (row, col) = firstEmpty board
--             validValues = filter (\value -> validRow board comparisionRows row col && validColumn board comparisionColumns row col && validBlock board row col) [1..9]
--             solutions = map (\value -> backtrack (replace board row col (Just value)) comparisionRows comparisionColumns) validValues
--         in listToMaybe (catMaybes solutions)