{-
Crie uma fun¸c˜ao que receba trˆes notas de um aluno (a, b, c), calcule a m´edia e retorne se o aluno foi
aprovado ou reprovado. Para um aluno ser aprovado, ele deve possuir nota igual ou superior a 6. Leia as
notas dos alunos do teclado.
-}

media :: Float -> Float -> Float -> Float
media n1 n2 n3 = (n1 + n2 + n3) / 3

resultado :: Float -> Float -> Float -> String
resultado a b c =
    if (media a b c) >= 6
        then "Aprovado"
        else "Reprovado"

main :: IO()
main = do
    putStr "Insira a primeira nota do aluno:\n>>"
    input <- getLine
    let n1 = read input:: Float

    putStr "Insira a segunda nota do aluno:\n>>"
    input <- getLine
    let n2 = read input:: Float

    putStr "Insira a terceira nota do aluno:\n>>"
    input <- getLine
    let n3 = read input:: Float

    putStrLn ("O aluno está " ++ resultado n1 n2 n3 ++ ".")