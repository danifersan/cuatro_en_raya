    -- Práctica Haskell

--El primer Int es la columna donde la metemos. a es el jugador que juega.
-- Tablero inicial y tablero final.
meter_ficha:: Int -> Int -> [[Int]] -> [[Int]]
meter_ficha m x tablero = meter_ficha' m x tablero 0

-- Lo que hace esta función es recorrer el tablero desde abajo hasta
-- arriba poniendo la ficha en el primer lugar vacío.
meter_ficha':: Int -> Int -> [[Int]] -> Int -> [[Int]]
meter_ficha' m x tablero i
--	| (length (tablero) < i) = tablero
	| (tablero!!i!!m == 0) = ponerFicha m i x tablero
	| otherwise = meter_ficha' m x tablero (i+1)
	
-- poner ficha lo que hace es copiar en nuevo la amtriz de tablero modificando la casilla m i.
ponerFicha :: Num a => Int -> a -> b -> [[b]] -> [[b]]
ponerFicha x y nuevo_elem tablero = ponerFichaEnLista tablero x (ponerFichaEnLista (tablero!!x) y nuevo_elem 0) 0

ponerFichaEnLista :: Num a => [b] -> a -> b -> a -> [b]
ponerFichaEnLista [] x nuevo_elem act_x = []
ponerFichaEnLista (y:ys) x nuevo_elem act_x
	| act_x == x = (nuevo_elem:ys)
	| otherwise = (y:ponerFichaEnLista ys x nuevo_elem (act_x+1))
    
    
-- Dada una lista de listas, y una posición (x,y), cambiamos esa posición
cambiar l (x,y) nuevo_elem = k ++ [i ++ [nuevo_elem] ++ (tail j)] ++ (tail m)
    where (k, m) = splitAt x l
          (i, j) = splitAt y (l!!y)