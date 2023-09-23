import ComparisonSudoku

main :: IO ()
main = do

    let board :: Board = replicate 9 (replicate 9 0)
    let comparisionRows :: ComparisonRows = [[Less, Greater, Less, Less, Greater, Greater],
                                              [Less, Less, Less, Greater, Greater, Less],
                                              [Less, Less, Less, Greater, Greater, Less],
                                              [Greater, Greater, Greater, Less, Less, Greater],
                                              [Greater, Less, Greater, Less, Greater, Greater],
                                              [Less, Greater, Less, Greater, Greater, Less],
                                              [Less, Greater, Greater, Greater, Less, Greater],
                                              [Greater, Less, Greater, Less, Less, Less],
                                              [Less, Greater, Less, Less, Less, Greater]]

    let comparisionColumns :: ComparisonColumns = [[Greater, Less, Greater, Greater, Greater, Less],
                                                    [Less, Less, Less, Less, Less, Greater, Less],
                                                    [Less, Less, Less, Less, Greater, Greater],
                                                    [Greater, Less, Greater, Less, Less, Greater],
                                                    [Greater, Greater, Greater, Less, Greater, Greater],
                                                    [Greater, Greater, Greater, Greater, Greater, Less],
                                                    [Less, Greater, Less, Greater, Less, Greater],
                                                    [Greater, Greater, Greater, Greater, Less, Less],
                                                    [Less, Greater, Less, Greater, Less, Greater]]

    let solution :: Board = [[3, 4, 2, 6, 8, 9, 7, 5, 1],
                             [1, 5, 6, 3, 7, 2, 8, 4, 9],
                             [7, 8, 9, 4, 5, 1, 3, 2, 6],
                             [9, 2, 1, 5, 4, 7, 6, 8, 3],
                             [8, 3, 4, 2, 1, 6, 9, 7, 5],
                             [6, 7, 5, 8, 9, 3, 2, 1, 4],
                             [4, 9, 8, 7, 6, 5, 1, 3, 2],
                             [2, 1, 7, 9, 3, 4, 5, 6, 8],
                             [5, 6, 3, 1, 2, 8, 4, 9, 7]]
    -- if validBoard solution then printBoard solution else putStrLn "No solution found."
    let upComparator = getComparatorUp comparisionColumns 0 0
    let downComparator = getComparatorDown comparisionColumns 0 0
    let leftComparator = getComparatorLeft comparisionRows 0 0
    let rightComparator = getComparatorRight comparisionRows 0 0

    if upComparator == None then putStrLn "upComparator == None" else putStrLn ""
    if downComparator == Greater then putStrLn "downComparator == Greater" else putStrLn ""
    if leftComparator == None then putStrLn "leftComparator == None" else putStrLn ""
    if rightComparator == Less then putStrLn "rightComparator == Greater" else putStrLn ""
    if getCell solution 0 0 == 3 then putStrLn "getCell solution 0 0 == 3" else putStrLn ""
    if isFull solution then putStrLn "isFull solution" else putStrLn ""
    if validRow solution comparisionRows 0 0 then putStrLn "validRow solution comparisionRows 0 0" else putStrLn ""
    if validColumn solution comparisionColumns 0 0 then putStrLn "validColumn solution comparisionColumns 0 0" else putStrLn ""
    -- let solution :: Maybe Board = backtrack board comparisionRows comparisionColumns
    -- case solution of
    --     Just board -> printBoard board
    --     Nothing -> putStrLn "No solution found."