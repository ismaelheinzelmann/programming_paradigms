-- Crie uma fun¸c˜ao que receba dois n´umeros x e y e retorne xy . Leia x e y do teclado.

potencia :: Float -> Float -> Float
potencia x y = x ** y

main :: IO ()
main = do
  print "Insira o valor de x"
  input <- getLine
  let x = read input :: Float

  print "Insira o valor de y"
  input <- getLine
  let y = read input :: Float

  putStr "O valor de x^y é: "
  print (potencia x y)
