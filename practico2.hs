-- Función recursiva de sumatoria
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- Función recursiva de factorial
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

-- Función de sumatoria para un elemento
sumatoria2 :: Int -> Int
sumatoria2 x = sum [1..x]

-- 3)a)
soloPares :: [Int]->[Int]
soloPares [] = []
soloPares (x:xs) | x`mod`2==0 = x : soloPares xs 
		 | otherwise = soloPares xs

-- Discrepo
discrepo :: String 
discrepo = "discrepo"

-- Función length custom con recursividad
customLength :: [a] -> Int
customLength [] = 0
customLength (x:xs) = 1 + customLength xs

-- 3)b)
mayoresQue10 :: [Int] -> [Int]
mayoresQue10 []=[]
mayoresQue10 (x:xs)|x>10=x:mayoresQue10 xs
		   |otherwise= mayoresQue10 xs

-- Prueba de función map
sumar3 :: [Int] -> [Int]
sumar3 [] = []
sumar3 (x : xs) = (x+3) : sumar3 xs

-- 3)c)
mayoresQue :: Int -> [Int] -> [Int]
mayoresQue x [] = []
mayoresQue x (y:ys) 	| x < y = y : mayoresQue x ys
			| otherwise = mayoresQue x ys

-- Funciones Recursivas del tipo Map
-- 4)a)
sumar1::[Int]->[Int]
sumar1 []=[]
sumar1 (x:xs)=(x+1) : sumar1 xs

-- 4)b)
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x * 2) : duplica xs

-- 4)c)
multiplica :: Int -> [Int] -> [Int]
multiplica n []=[]
multiplica n (x:xs) = (n*x):multiplica n xs

-- Funciones Recursivas del tipo Fold
-- 5)a)
todosMenores10 :: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (x:xs) = (x<10) == todosMenores10 xs

	
