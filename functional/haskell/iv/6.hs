import Distribution.Simple.GHC (getLibDir)
import System.Posix.Internals (puts)
import Text.Parsec (putState)
{-
Crie uma fun¸c˜ao que receba trˆes inteiros x, y e z e retorne se havendo varetas com esses valores em
comprimento pode-se construir um triˆangulo. Exemplo, com varetas de comprimento 4, 8 e 9 posso
construir um triˆangulo, por´em com varetas de comprimento 10, 5 e 4 n˜ao posso construir um triˆangulo.
Leia x, y e z do teclado.
-}

valid:: Float -> Float -> Float -> Bool
valid a b c = a > abs b - c

result:: Float -> Float -> Float -> Bool
result a b c = valid a b c && valid b c a && valid c a b

main:: IO()
main = do
    putStr "Insira o valor de a:\n>>"
    input <- getLine
    let a = read input::Float

    putStr "Insira o valor de b:\n>>"
    input <- getLine
    let b = read input::Float

    putStr "Insira o valor de c:\n>>"
    input <- getLine
    let c = read input::Float
    
    putStrLn ("O triângulo é " ++ (if result a b c then "válido" else "inválido"))