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

-- Discrepo
discrepo :: String 
discrepo = "Ejecuto una discrepancia"

-- Funciones Recursivas del tipo Filter
-- 3)a)
soloPares :: [Int]->[Int]
soloPares [] = []
soloPares (x:xs)  | x `mod` 2 == 0 = x : soloPares xs 
		              | otherwise = soloPares xs


-- 3)b)
mayoresQue10 :: [Int] -> [Int]
mayoresQue10 []=[]
mayoresQue10 (x:xs) | x > 10 = x : mayoresQue10 xs
		                |otherwise = mayoresQue10 xs


-- 3)c)
mayoresQue :: Int -> [Int] -> [Int]
mayoresQue x [] = []
mayoresQue x (y:ys) | x < y = y : mayoresQue x ys
                    | otherwise = mayoresQue x ys

-- Funciones Recursivas del tipo Map

-- Prueba de función map
sumar3 :: [Int] -> [Int]
sumar3 [] = []
sumar3 (x : xs) = (x+3) : sumar3 xs

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

-- 5)b)
hay0::[Int]->Bool
hay0 []=False
hay0 (x:xs)=(x==0)||hay0 xs

-- 5)c)	
sumC :: [Int] -> Int
sumC [] = 0
sumC (x:xs) = x + sumC xs

-- Funciones Recursivas del tipo Zip
-- 6)
repartir :: [String] -> [String] ->[(String, String)]
repartir (x:xs) [] = (x,"No queda mas nada para vos pichon :'c") :repartir xs []
repartir [] _ = []
repartir (x:xs) (y:ys) = (x, y) : repartir xs ys

-- Funciones Recursivas de tipo Unzip
--7)
apellido::[(String,String,Int)]->[String]
apellido []=[]
apellido ((x,y,z):xs) = y:apellido xs

-- 8) length escrito de forma recursiva
customLength :: [a] -> Int
customLength [] = 0
customLength (x:xs) = 1 + customLength xs

-- 8) !! escrito de forma recursiva
customId :: [a] -> Int -> a
customId (x:xs) 0 = x
customId (x:xs) n = customId xs (n-1)

--8) take escrito de forma recursiva
customTake:: Int -> [a] -> [a]
customTake n _ | n <= 0 = []
customTake _ [] = []
customTake n (x:xs) = x : customTake (n-1) xs

--8) drop escrito de forma recursiva
customDrop :: Int -> [a] -> [a]
customDrop n xs | n <= 0 = xs 
customDrop _ [] = []
customDrop n (x:xs) = customDrop (n-1) xs

--8) ++ escrito de forma recursiva
customConcat :: [a] -> [a] -> [a]
customConcat [] ys = ys
customConcat (x:xs) ys = x : customConcat xs ys

--9)a) Función Fold
maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

--9)b) Función Fold
sumaPares :: [(Int, Int)] -> Int
sumaPares [] = 0
sumaPares ((x,y):xs) = x + y + sumaPares xs

--9)c) Función Fold
todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x:xs) | x /= 1 && x /= 0 = False
                | otherwise = True && todos0y1 xs

--9)d) Función Filter
quitar0s :: [Int] -> [Int]
quitar0s [] = []
quitar0s (x:xs) | x == 0 = quitar0s xs
                | otherwise = x : quitar0s xs

--9)e) Función Fold
ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

--9)f)
repetir :: Int -> Int -> [Int]
repetir 0 _ = []
repetir n k = k : repetir (n-1) k

--9)g) Función Fold
concate :: [[a]] -> [a]
concate [] = []
concate (x:xs) = x ++ (concate xs)

--9)h) Función Map
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]
