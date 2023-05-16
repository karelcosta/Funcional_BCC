-- QUESTÃO 1

--Alternativa A
impares1a100 :: [Int]
impares1a100  = imparesNaM 1 100
imparesNaM :: Int -> Int -> [Int]
imparesNaM n m = [a | a <- [n..m], mod a 2 /= 0]

--Alternativa B
pares10a100::  [Int]
pares10a100 = paresNaM 10 100
paresNaM :: Int -> Int -> [Int]
paresNaM n m = [a | a <- [n..m], mod a 2 == 0]

--Alternativa C
impares1an :: Int -> [Int]
impares1an n = imparesNaM 1 n


--Alternativa D
-- d. Números entre 1 e N que são múltiplos de 3 e 5 ao mesmo tempo.
multiplos3e5_1aN :: Int -> [Int]
multiplos3e5_1aN n = multiplos3e5_NaM 1 n 
multiplos3e5_NaM :: Int ->Int -> [Int]
multiplos3e5_NaM n m = [a | a <- [n..m], mod a 3 == 0 && mod a 5 == 0]

--Alternativa E
-- e. Tuplas entre 1 e N, contendo o número e seu respectivo quadrado.
tuplas_1aN :: Int -> [(Int, Int)]
tuplas_1aN n = [(a, a*a)| a<-[1..n]]

--Alternativa F
-- f. Tuplas com os índices de uma matriz 3x4.
tuplasMatriz3x4 :: [(Int,Int)]
tuplasMatriz3x4 = tuplasMatrizNxM 3 4

--Alternativa G
-- g. Tuplas com os índices de uma matriz NxM.
tuplasMatrizNxM::Int -> Int -> [(Int,Int)]
tuplasMatrizNxM n m = [(l, c)| l<-[1..n], c<-[1..m]]



-- QUESTÃO 2
--Escreva uma função com a seguinte assinatura listaFibonacci :: Int->[Int] que retorna uma lista
-- com os n primeiros números da sequência de Fibonacci.
listaFibonacci:: Int->[Int]
listaFibonacci n = [fib a|a<-[1..n]]

fib :: Int -> Int -- retorna o n-ésimo número da sequência de fibonachi 
fib n
    | n == 0 = 0
    | n <= 2 = 1
    |otherwise = fib (n-1) + fib (n-2)


-- QUESTÃO 3
-- O sistema de numeração hexadecimal é muito utilizado para representar números binários de
-- uma forma mais compacta, pois é muito fácil converter binários para hexadecimal e vice-versa.
-- Dessa forma, esse sistema é bastante utilizado em aplicações de computadores e
-- microprocessadores. Sabemos que o sistema de numeração binário (base 2) possui apenas
-- dois valores (0 e 1) para cada casa numérica. Já o sistema de numeração hexadecimal (base 16)
-- possui dezesseis possíveis valores (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F). Escreva uma função
-- em Haskell que receba uma String em binário e retorne uma String com sua representação
-- hexadecimal.

binario_hexadecimal :: [Char] -> [Char]
binario_hexadecimal n = [   |a<-n, == ]