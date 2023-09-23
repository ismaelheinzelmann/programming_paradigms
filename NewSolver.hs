import Data.List
import Data.Maybe

-- Define the Vergleichssudoku board as a list of lists with cells containing Maybe Int.
type Board = [[Maybe Int]]

-- Define the size of the Vergleichssudoku board (usually 9x9).
boardSize :: Int
boardSize = 9

-- Define comparison functions for different symbols.
data Comparison = Less | Greater deriving (Eq, Show)

-- Function to print the Vergleichssudoku board.
printBoard :: Board -> IO ()
printBoard = mapM_ (putStrLn . intercalate " " . map showCell)

-- Function to display a cell value or a comparison symbol.
showCell :: Maybe Int -> String
showCell Nothing    = " "
showCell (Just num) = show num

-- Function to check if a number placement satisfies a comparison.
isValid :: Comparison -> Int -> Int -> Bool
isValid Less    a b = a < b
isValid Greater a b = a > b

-- Function to check if a number is valid to place at a given position on the board.
isValidPlacement :: Board -> (Int, Int) -> Int -> Bool
isValidPlacement board (row, col) num =
    all (isValidComparison board (row, col) num) $ neighbors (row, col)
  where
    isValidComparison :: Board -> (Int, Int) -> Int -> (Comparison, Maybe Int) -> Bool
    isValidComparison _ _ _ (_, Nothing)     = True
    isValidComparison _ _ _ (_, Just _)      = True
    isValidComparison board (r, c) n (comp, Just neighbor) =
        case board !! r !! c of
            Nothing   -> True
            Just cell ->
                if comp == Less
                    then cell < neighbor
                    else cell > neighbor

-- Function to find neighboring cells for comparisons.
neighbors :: (Int, Int) -> [(Comparison, Maybe Int)]
neighbors (row, col) =
    [(Less, getCell (row - 1, col)),
     (Greater, getCell (row + 1, col)),
     (Less, getCell (row, col - 1)),
     (Greater, getCell (row, col + 1))]
  where
    getCell :: (Int, Int) -> Maybe Int
    getCell (r, c)
        | r >= 0 && r < boardSize && c >= 0 && c < boardSize = board !! r !! c
        | otherwise = Nothing

-- Function to find an empty position on the board.
findEmpty :: Board -> Maybe (Int, Int)
findEmpty board =
    find (\(row, col) -> isNothing (board !! row !! col)) [(row, col) | row <- [0..(boardSize - 1)], col <- [0..(boardSize - 1)]]

-- Vergleichssudoku solver using backtracking.
solveVergleichssudoku :: Board -> Maybe Board
solveVergleichssudoku board =
    case findEmpty board of
        Nothing -> Just board  -- If no empty cells are found, the puzzle is solved.
        Just (row, col) ->
            -- Try placing each valid number (1-9) in the empty cell.
            let validNums = filter (isValidPlacement board (row, col)) [1..boardSize]
                tryNumber num = case solveVergleichssudoku (replace2D board row col (Just num)) of
                    Just solution -> Just solution
                    Nothing       -> Nothing
            in
                listToMaybe $ mapMaybe tryNumber validNums
  where
    -- Function to replace an element at a specific row and column in a 2D list.
    replace2D :: [[a]] -> Int -> Int -> a -> [[a]]
    replace2D lst r c val = take r lst ++ [take c (lst !! r) ++ [val] ++ drop (c + 1) (lst !! r)] ++ drop (r + 1) lst

main :: IO ()
main = do
    let vergleichssudoku = [ [Just 1, Just 2, Nothing, Nothing, Nothing, Nothing, Nothing, Just 7, Nothing]
                           , [Nothing, Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Nothing]
                           , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                           , [Nothing, Nothing, Nothing, Nothing, Just 4, Nothing, Just 3, Nothing, Nothing]
                           , [Nothing, Nothing, Nothing, Just 6, Nothing, Just 7, Nothing, Nothing, Nothing]
                           , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 1, Nothing, Nothing]
                           , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 5, Nothing]
                           , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                           , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                           ]

    putStrLn "Input Vergleichssudoku:"
    printBoard vergleichssudoku
    putStrLn "\nSolved Vergleichssudoku:"
    case solveVergleichssudoku vergleichssudoku of
        Just solution -> printBoard solution
        Nothing       -> putStrLn "No solution found."
