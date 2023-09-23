import ComparisonSudoku
import Text.XHtml (background)

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
    let solution :: Maybe Board = solveSudokuBacktracking board comparisionRows comparisionColumns
    case solution of
        Just board -> printBoard board
        Nothing -> putStrLn "No solution found."