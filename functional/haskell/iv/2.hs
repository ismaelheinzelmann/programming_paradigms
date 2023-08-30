-- Crie uma fun¸c˜ao que receba um n´umero x, negativo ou positivo, e retorne seu valor absoluto. Leia x do teclado.
modulus :: Int -> Int
modulus x =
  if x < 0
    then x * (-1)
    else x

main :: IO ()
main = do
  putStrLn "Insira o valor de x:"
  putStr ">>"
  input <- getLine
  let x = read input :: Int

  putStrLn "O módulo de x é: "
  print (modulus x)