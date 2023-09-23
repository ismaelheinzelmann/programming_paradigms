import ComparisonSudoku

main :: IO ()
main = do
    let board :: Board = replicate 9 (replicate 9 0)
    let comparisionRows :: ComparisonRows = [[Less, Greater, None,Less, Less,None, Greater, Greater],
                                              [Less, Less, None, Less, Greater,None, Greater, Less],
                                              [Less, Less,None, Less, Greater,None, Greater, Less],
                                              [Greater, Greater,None, Greater, Less,None, Less, Greater],
                                              [Greater, Less,None, Greater, Less,None, Greater, Greater],
                                              [Less, Greater,None, Less, Greater,None, Greater, Less],
                                              [Less, Greater,None, Greater, Greater,None, Less, Greater],
                                              [Greater, Less,None, Greater, Less,None, Less, Less],
                                              [Less, Greater,None, Less, Less,None, Less, Greater]]
    let comparisionColumns :: ComparisonColumns = [[Greater, Less, None,Greater, Greater,None, Greater, Less],
                                                    [Less, Less,None, Less, Less,None, Greater, Less],
                                                    [Less, Less,None, Less, Less,None, Greater, Greater],
                                                    [Greater, Less,None, Greater, Less,None, Less, Greater],
                                                    [Greater, Greater,None, Greater, Less,None, Greater, Greater],
                                                    [Greater, Greater,None, Greater, Greater,None, Greater, Less],
                                                    [Less, Greater,None, Less, Greater,None, Less, Greater],
                                                    [Greater, Greater,None, Greater, Greater,None, Less, Less],
                                                    [Less, Greater,None, Less, Greater,None, Less, Greater]]
    let solution :: Board = [[3, 4, 2, 6, 8, 9, 7, 5, 1],
                             [1, 5, 6, 3, 7, 2, 8, 4, 9],
                             [7, 8, 9, 4, 5, 1, 3, 2, 6],
                             [9, 2, 1, 5, 4, 7, 6, 8, 3],
                             [8, 3, 4, 2, 1, 6, 9, 7, 5],
                             [6, 7, 5, 8, 9, 3, 2, 1, 4],
                             [4, 9, 8, 7, 6, 5, 1, 3, 2],
                             [2, 1, 7, 9, 3, 4, 5, 6, 8],
                             [5, 6, 3, 1, 2, 8, 4, 9, 7]]
    if validBoard solution comparisionRows comparisionColumns
        then putStrLn "Solution is valid."
        else putStrLn "Solution is not valid."
    let solution :: Maybe Board = backtrack board comparisionRows comparisionColumns
    case solution of
        Just board -> printBoard board
        Nothing -> putStrLn "No solution found."