import Text.Read (Lexeme(Char))
{-
Crie uma fun¸c˜ao que receba trˆes parˆametros Operador, x e y, e retorne o resultado da opera¸c˜ao matem´atica
x Operador y. Os operadores poss´ıveis de informar s˜ao +, -, *, /. Leia o Operador, x e y do teclado.
-}

operate :: String -> Float -> Float -> Float
operate operador x y
    | operador == "+" = x + y
    | operador == "-" = x - y
    | operador == "*" = x * y
    | operador == "/" = x / y
    | otherwise = 0

main :: IO()
main = do
    putStr "Insira o operador:\n>>"
    input <- getLine
    let op = input

    putStr "Insira o primeiro operando:\n>>"
    input <- getLine
    let x = read input:: Float

    putStr "Insira o segundo operando:\n>>"
    input <- getLine
    let y = read input:: Float
    
    putStrLn ("O valor da operação é " ++ show (operate op x y))

