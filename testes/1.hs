primoaux::Int->Int->Bool
primoaux n naux
    |naux == 1 = True
    |mod n naux == 0 = False
    |otherwise = primoaux n (naux - 1)

primo :: Int -> Bool
primo n
    | n <= 1 = False
    | otherwise = primoaux n (n-1)

primogemeo::Int->Int->Bool
primogemeo na nb
    |primo na == False || primo nb == False = False
    |nb - na == 2 = True
    |otherwise = False

b::Int->Bool
b n
    |primogemeo n (n+2) || primogemeo (n-2) n = True
    |otherwise = False

c::Int->Int
c n
    |n<5=0
    |primogemeo (n-2) n == True = 1 + c (n-1)
    |otherwise = c (n-1)

somarprimosgemeos::Int->Int
somarprimosgemeos n
    |n<5=0
    |primogemeo (n-2) n == True = n + (n-2) + somarprimosgemeos (n-1)
    |otherwise = somarprimosgemeos (n-1)


algarismos :: Int -> Int -- conta o numero de algarismos 
algarismos n 
    | n < 10 = 1
    | otherwise = 1 + algarismos (div n 10)

fat :: Int -> Int --calcula o fatorial 
fat n 
    | n == 0 = 1
    | otherwise = n * fat (n - 1)

invfat :: Int -> Int -- faz o caminho inverso do fatorial 
invfat n 
    | n == 1 = 1
    | otherwise = invfataux n 1

invfataux :: Int -> Int -> Int
invfataux n x
    | fat x == n = x 
    | fat x > n = -1
    | otherwise = invfataux n (x + 1)

somafat :: Int -> Int -- soma todos os fatoriais entre n e 0
somafat n
    | n == 0 = 1
    | otherwise = fat n + somafat (n-1)

fib :: Int -> Int -- retorna o n-ésimo número da sequência de fibonachi 
fib n
    | n == 0 = 0
    | n <= 2 = 1
    |otherwise = fib (n-1) + fib (n-2)

prodfib :: Int -> Int -- retorna o produto dos n primeiros números da sequência de fibonachi
prodfib n
    | n == 0 = 1
    | otherwise = fib n * prodfib (n-1)

primofib :: Int -> Int --retorna o n-ésimo número primo da sequência de fibonachi
primofib n
    | n == 1 = 2
    | otherwise = auxprimofib n 1

auxprimofib :: Int -> Int -> Int 
auxprimofib n k 
    | n == 1 && primo (fib k) = fib k
    | primo (fib k) = auxprimofib (n-1) (k+1)
    |otherwise = auxprimofib n (k+1)