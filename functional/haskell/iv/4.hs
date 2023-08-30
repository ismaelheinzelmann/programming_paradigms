{-
Crie uma funçãoao que receba dois valores booleanos (x, y) retorne o resultado do “ou exclusivo” (XOR)
sobre eles. A função apenas deve usar os operadores &&, || e not. Leia os valores x e y do teclado.
-}

xorFunc :: Bool -> Bool -> Bool
xorFunc x y = (x && not y) || (not x && y)

main :: IO ()
main = do
  putStrLn "Insira o valor de x:"
  putStr ">>"
  input <- getLine
  let x = read input :: Bool

  putStrLn "Insira o valor de y:"
  putStr ">>"
  input <- getLine
  let y = read input :: Bool

  putStrLn "O resultado do XOR é: "
  print (xorFunc x y)