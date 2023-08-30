-- Crie uma fun¸c˜ao que compute o n-´esimo n´umero de Fibonacci. Leia n do teclado.

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = (n - 1) + (n - 2)

main:: IO()
main = do
    putStr "Insira n para calcular o n-ésimo valor de Fibonacci:\n>>"
    input <- getLine
    let n = read input::Int

    putStrLn ("O n-ésimo valor de Fibonacci é " ++ show (fib n))
