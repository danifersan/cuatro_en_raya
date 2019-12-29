-- Dada una lista de listas, y una posición (x,y), cambiamos esa posición
cambiar :: [[a]] -> (Int,Int) -> a -> [[a]]
cambiar l (x,y) nuevo_elem = k ++ [i ++ [nuevo_elem] ++ (tail j)] ++ (tail m)
    where (k, m) = splitAt x l
          (i, j) = splitAt y (l!!y)

encuentraVacio :: Num a => [[a]] -> Int -> a -> [[a]]
encuentraVacio tablero colum elem = encuentraVacio' tablero (colum -1) elem ((length tablero) -1)

encuentraVacio' :: Num a => [[a]] -> Int -> a -> Int -> [[a]]
encuentraVacio' tablero colum elem l
	| tablero!!l!!colum == 0 = cambiar tablero (l,colum) elem
	| l<0 = tablero
	| otherwise = encuentraVacio' tablero colum elem (l-1)
	
se_puede_meter :: [[Int]] -> Int -> Int -> Bool
se_puede_meter tablero columna elemento = se_puede_meter' tablero (columna - 1) elemento ((length tablero) -1) 

se_puede_meter' :: [[Int]] -> Int -> Int -> Int -> Bool
se_puede_meter' tablero columna elemento n
	| n<0 = False
	| tablero!!columna!!n == 0 = True
	| otherwise = se_puede_meter' tablero columna elemento (n-1) 