type Matriz = [[Int]]

expandir:: Matriz -> [Matriz]
expandir [] = [[]]
expandir matriz = meter_hasta_el_final matriz 1 2

en_rango :: Int -> Matriz -> Bool
en_rango columna matriz = columna <= length(matriz!!1)

en_rango_test = -- debe dar true
    en_rango (length(a)) a
        where a = [[1,2,0,0],[1,1,0,0],[1,2,0,1],[2,2,2,2]]
{--
    Matriz a
    1 2 0 0
    1 1 0 0
    1 2 0 1
    2 2 2 2
--}

-- METER HASTA EL FINAL
meter_hasta_el_final:: [[Int]] -> Int -> Int -> [[[Int]]]
meter_hasta_el_final matriz columna elemento
    |en_rango columna matriz && (se_puede_meter matriz columna elemento) = [encuentraVacio matriz columna elemento] ++ (meter_hasta_el_final matriz (columna+1) elemento)
    |en_rango columna matriz = (meter_hasta_el_final matriz (columna+1) elemento)
    |otherwise = []

-- SE PUEDE METER
se_puede_meter :: [[Int]] -> Int -> Int -> Bool
se_puede_meter tablero columna elemento = se_puede_meter' tablero (columna - 1) elemento ((length tablero) -1) 

se_puede_meter' :: [[Int]] -> Int -> Int -> Int -> Bool
se_puede_meter' tablero columna elemento n
    | n<0 = False
    | tablero!!columna!!n == 0 = True
    | otherwise = se_puede_meter' tablero columna elemento (n-1) 



-- Dada una lista de listas, y una posición (x,y), cambiamos esa posición
cambiar :: [[a]] -> (Int,Int) -> a -> [[a]]
cambiar l (x,y) nuevo_elem = k ++ [i ++ [nuevo_elem] ++ (tail j)] ++ (tail m)
    where (k, m) = splitAt x l
          (i, j) = splitAt y (l!!y)

--encuentraVacio :: Num a => [[a]] -> Int -> a -> [[a]]
encuentraVacio tablero colum elem = encuentraVacio' tablero (colum -1) elem ((length tablero) -1)

--encuentraVacio' :: Num a => [[a]] -> Int -> a -> Int -> [[a]]
encuentraVacio' tablero colum elem l
    | (tablero!!l!!colum) == 0 = cambiar tablero (l,colum) elem
    | l<0 = tablero
    | otherwise = encuentraVacio' tablero colum elem (l-1)
    
expandir_test =
    expandir a
        where a = [[1,2,0,0],[1,1,0,0],[1,2,0,1],[2,2,2,2]]
   
meter_hasta_el_final_test =
    meter_hasta_el_final a 1 2
        where a = [[1,2,0,0],[1,1,0,0],[1,2,0,1],[2,2,2,2]]


transpuesta :: Matriz -> Matriz
transpuesta [] = []
transpuesta [vector] = map (:[]) vector
transpuesta (v1:vs)  = zipWith (:) v1 (transpuesta vs)

