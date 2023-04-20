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

