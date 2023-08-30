gcd' :: Int -> Int -> Int
gcd' a b =
    if a == 0 then b
    else
        gcd' (b `mod` a) a

phi :: Int -> Int
phi n = length [x | x <- [1..n-1], gcd' n x == 1]

-- x tal q o maior divisor comum de x onde x = 1 .. n-1 seja igual a 1
-- https://wiki.haskell.org/List_comprehension
-- [(i,j) | i <- [1,2], j <- [1..4] ]
-- [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4)]

-- take 5 [ [ (i,j) | i <- [1,2] ] | j <- [1..] ]
-- [[(1,1),(2,1)], [(1,2),(2,2)], [(1,3),(2,3)], [(1,4),(2,4)], [(1,5),(2,5)]]

main :: IO ()
main = do
    putStrLn "Digite um número n:"
    n <- readLn
    let result = phi n
    putStrLn ("φ(" ++ show n ++ ") = " ++ show result)
