--Funciones principales:

-- esta_llena_matriz :: [[Int]] -> Bool Devuelve si esta lleno el tablero; osea si se puede seguir jugando

-- traspuesta Dada una matriz devuelve la matriz traspuesta

-- victoria :: Matriz -> Bool; Dado un tablero devuelve si has ganado o no

-- encuentraVacio :: [[Int]] -> Int -> Int -> [[Int]]    --Mete el elemento en la columna segun las reglas del coneta 4
--                   Tablero  Columna  Elemento            Si no puede meter la ficha en esa posicion devuelve fallo (CAMBIAR)

--  minimaxMain prof expandir evaluar problema
    -- Expandir: es una funcion que dado un tablero te devuelve la lista de posibles tableros, osea los 7 siguientes tableros (por ejemplo)
    -- Evaluar: Dado un tablero dice que tan de bueno es para mi (elegimos un valor)
    -- Profundidad: Cuantas jugadas quieres que vaya por delante
    -- Problema: Estado actua

--se_puede_meter :: [[Int]] -> Int -> Int -> Bool            
--                  Tablero Elemento Columna



type Matriz = [[Int]]
type Vector = [Int]
----------------------------------------------------------------------------------------------------------------------------------------------------------
-- PRINCIPIO DEL JUEGO
menu::IO()
menu = do 
			putStrLn "1:Multijugador"
			putStrLn "2:Un jugador"
			putStrLn "3:Final"
			opcion <- getLine
			case read(opcion)::Int of
				1 -> do 
						comienza_multijugador
						menu
				2 -> do 
						comienza_unJugador
						menu
				3 -> do 
						putStr "Hasta luego."
						
----------------------------------------------------------------------------------------------------------------------------------------------------------
-- FASE MULTIJUGADOR
comienza_multijugador :: IO ()
comienza_multijugador = do 
							putStr "¿Cuantas filas quieres jugar?"
							dimensionF <- getLine
							putStr "¿Y columnas?"
							dimensionC <- getLine
							let imprimirTablero = crearTablero (read(dimensionF)::Int) (read(dimensionC)::Int)
							print imprimirTablero
							multijugador imprimirTablero 

multijugador :: [[Int]] -> IO() 
multijugador tablero = do
							putStrLn "¿Que jugador eres?"
							player <- getLine
							putStrLn "¿En que columna quieres poner?"
							column <- getLine
							let continuar = continua tablero (read(column)::Int) (read(player)::Int) 
							print continuar
							multijugador continuar
							
----------------------------------------------------------------------------------------------------------------------------------------------------------
-- FASE UNJUGADOR
comienza_unJugador :: IO ()
comienza_unJugador = do 
						putStr "¿Cuantas filas quieres jugar?"
						dimensionF <- getLine
						putStr "¿Y columnas?"
						dimensionC <- getLine
						putStrLn "¿Que nivel de dificultad quieres?"
						putStrLn "1-Facil"
						putStrLn "2-Medio"
						putStrLn "3-Dificil"
						nivel <- getLine
						putStrLn "Recuerda que tu eres el jugador 1 y la maquina el 2"
						let imprimirTablero = crearTablero (read(dimensionF)::Int) (read(dimensionC)::Int)
						print imprimirTablero
						unJugador imprimirTablero (read(nivel)::Int)

unJugador :: [[Int]] -> Int -> IO() 
unJugador tablero nivel = do
							putStrLn "¿En que columna quieres poner?"
							column <- getLine
							let continuar_unJugador = continua tablero (read(column)::Int) 1 
							let Just maquina = (minimaxMain nivel expandir evaluar continuar_unJugador) 
							print maquina
							unJugador maquina nivel
							
----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Aquí queda ver la función que no de error nunca la funcion (encuentraVacio tablero column player). Va a devolver un tablero en cualquier caso.
continua :: [[Int]] -> Int -> Int ->  [[Int]]
continua tablero column player 
	| (victoria (tablero1)) || (esta_llena_matriz tablero1) = tablero1
	| otherwise = tablero1
		where tablero1 = encuentraVacio tablero column player




-- CREAR TABLERO
crearTablero :: Int -> Int -> [[Int]]
crearTablero filas columnas = take filas (repeat (take columnas (repeat 0)))
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Esta llena matriz
--Esta funcion dada una matriz devuelve si esta llena o no esta llena (es decir tiene algun 0 en alguna posicion)
esta_llena_matriz :: [[Int]] -> Bool
esta_llena_matriz [] = True
esta_llena_matriz matriz = not (elem False (estan_llenas_filas matriz))

hay_hueco::[Int] -> [Bool]  --Transforma los huecos en True y los ocupados en False
hay_hueco[] = [False]
hay_hueco[1] = [False]
hay_hueco[2] = [False]
hay_hueco[0] = [True]
hay_hueco (x:xs) = (hay_hueco[x])++(hay_hueco xs)

esta_llena_fila ::[Int] -> Bool           --Ve si la fila esta llena o no y devuelve un booleano: True si la fila esta llena, False si la fila estaba no llena
esta_llena_fila[] = True
esta_llena_fila xs = not (elem True (hay_hueco xs))


estan_llenas_filas ::[[Int]] -> [Bool]  --Dada una matriz devuelve una lista diciendo si cada fila esta llena True o esta vacia False y ademas un True al final, que no tendra relevancia
estan_llenas_filas [] = [True]
estan_llenas_filas (xs:xss) = [(esta_llena_fila xs)] ++ (estan_llenas_filas xss)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Traspuesta de una matriz
transpuesta :: Matriz -> Matriz
transpuesta [] = []
transpuesta [vector] = map (:[]) vector
transpuesta (v1:vs)  = zipWith (:) v1 (transpuesta vs)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Ver has ganado en horizontal
linea_ganadora :: [Int] -> Bool
linea_ganadora (a:b:c:d:xs)
    |a == 1 && a == b && b == c && c == d = True  
    |a == 2 && a == b && b == c && c == d = True  
    |otherwise = linea_ganadora (b:c:d:xs)
linea_ganadora _ = False

lista_lineas_ganadoras :: [[Int]] -> [Bool]
lista_lineas_ganadoras [] = [False]
lista_lineas_ganadoras (xs:xss) = [linea_ganadora xs] ++ (lista_lineas_ganadoras xss)

has_ganado_horizontal :: Matriz ->  Bool   
has_ganado_horizontal matriz = elem True (lista_lineas_ganadoras matriz)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
-- ver si has ganado en vertical
has_ganado_vertical :: Matriz -> Bool
has_ganado_vertical matriz = has_ganado_horizontal (transpuesta(matriz))

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ver si has ganado en oblicuo NOROESTE -> SURESTE

has_ganado_oblicuo_NO_SE :: Matriz -> Bool
has_ganado_oblicuo_NO_SE matriz = elem True (lista_lineas_ganadoras(diagonales_principales matriz))

diagonalizar :: Matriz -> Int -> Int-> [Int] --Convierte la diagonal inferior en una lista desde la posicion i j
diagonalizar [] _ _ = []
diagonalizar matriz i j
    | i < length(matriz) && j < length(matriz!!0) =  [matriz!!i!!j] ++ diagonalizar matriz(i+1)(j+1)
    |otherwise = []


recorrer_j :: Matriz -> Int -> Int -> [[Int]]
recorrer_j [] _ _ = []
recorrer_j matriz i j 
    | j < length (matriz!!0) = [diagonalizar matriz i j] ++ recorrer_j matriz i (j+1)
    |otherwise = []

recorrer_i :: Matriz -> Int -> Int -> [[Int]]
recorrer_i [] _ _ = []
recorrer_i matriz i j
    | i < length(matriz) = [diagonalizar matriz i j] ++ recorrer_i matriz (i+1) j
    |otherwise = []

diagonales_principales :: Matriz -> [[Int]]
diagonales_principales matriz = (recorrer_i matriz 0 0) ++ (recorrer_j matriz 0 0)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ver si has ganado en oblicuo NORESTE -> SUROESTE
has_ganado_oblicuo_NE_SO :: Matriz -> Bool      
has_ganado_oblicuo_NE_SO matriz = has_ganado_oblicuo_NO_SE (map reverse matriz)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
victoria ::Matriz -> Bool
victoria matriz = (has_ganado_horizontal matriz) || (has_ganado_vertical matriz) || (has_ganado_oblicuo_NO_SE matriz) || (has_ganado_oblicuo_NE_SO matriz)


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--METER FICHA EN EL TABLERO

-- Dada una lista de listas, y una posición (x,y), cambiamos esa posición
--cambiar :: [[a]] -> (Int,Int) -> a -> [[a]]
cambiar l (x,y) nuevo_elem = k ++ [i ++ [nuevo_elem] ++ (tail j)] ++ (tail m)
    where (k, m) = splitAt x l
          (i, j) = splitAt y (l!!x)

--encuentraVacio :: Num a => [[a]] -> Int -> a -> [[a]]
encuentraVacio tablero colum elem = encuentraVacio' tablero (colum -1) elem ((length tablero) -1)

--encuentraVacio' :: Num a => [[a]] -> Int -> a -> Int -> [[a]]
encuentraVacio' tablero colum elem l
    | tablero!!l!!colum == 0 = cambiar tablero (l,colum) elem
    | l<0 = tablero
    | otherwise = encuentraVacio' tablero colum elem (l-1)
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- MINIMAX
minimaxMain prof expandir evaluar problema    --Expandir: es una funcion que dado un tablero te devuelve la lista de posibles tableros, osea los 7 siguientes tableros (por ejemplo)
    |null siguientes = Nothing                 -- Evaluar: Dado un tablero dice que tan de bueno es para mi (elegimos un valor)
    |otherwise = Just(fst(maximum' sigsVals))   -- Profundidad: Cuantas jugadas quieres que vaya por delante
        where siguientes = expandir problema    -- Problema: Estado actual
              valoraciones = map(minimax(prof -1) expandir evaluar maximum minimum) siguientes
              sigsVals = zip siguientes valoraciones

maximum' :: Ord b => [(a,b)] -> (a,b)        
maximum' = foldr1 max' 

max' (t1,v1) (t2,v2) 
    | v1 <= v2 = (t2,v2)
    |otherwise = (t1,v1)
    
--minimax:: Ord b => Int -> (a -> [a]) -> ->(b -> [b]) -> (b -> [b]) -> a -> b
minimax prof expandir evaluar peor mejor problema
    |prof == 0 ||  null siguientes = evaluar problema
    |otherwise = mejor (map(minimax (prof - 1) expandir evaluar mejor peor)siguientes)
        where siguientes = expandir problema

--funcion expandir
expandir:: Matriz -> [Matriz]
expandir [] = [[]]
expandir matriz = meter_hasta_el_final matriz 1 2

en_rango :: Int -> Matriz -> Bool
en_rango columna matriz = columna <= length(matriz!!1)

-- METER HASTA EL FINAL
meter_hasta_el_final:: [[Int]] -> Int -> Int -> [[[Int]]]
meter_hasta_el_final matriz columna elemento
    |en_rango columna matriz && (se_puede_meter matriz columna elemento) = [encuentraVacio matriz columna elemento] ++ (meter_hasta_el_final matriz (columna+1) elemento)
    |en_rango columna matriz = (meter_hasta_el_final matriz (columna+1) elemento)
    |otherwise = []


-- SE PUEDE METER
se_puede_meter :: [[Int]] -> Int -> Int -> Bool
se_puede_meter matriz columna elemento = head matriz!!(columna-1) == 0

diagonales_secundarias :: Matriz -> [[Int]]
diagonales_secundarias matriz = diagonales_principales (map reverse matriz)

evaluar :: [[Int]] -> Int
evaluar matriz = (evaluar2 matriz) - (evaluar1 matriz)



evaluar2 :: [[Int]] -> Int
evaluar2 matriz =
    (
    ((10^4)*(numero_cuatro_2_seguidos_en_matriz matriz))+
    ((10^3)*(numero_tres_2_seguidos_en_matriz matriz)) +
    ((10^2)*(numero_dos_2_seguidos_en_matriz matriz)) +
    ((10)*(numero_uno_2_seguidos_en_matriz matriz))
    )
    
    
evaluar1 :: [[Int]] -> Int
evaluar1 matriz =
    (
    ((10^4)*(numero_cuatro_1_seguidos_en_matriz matriz))+
    ((10^3)*(numero_tres_1_seguidos_en_matriz matriz)) +
    ((10^2)*(numero_dos_1_seguidos_en_matriz matriz)) +
    ((10)*(numero_uno_1_seguidos_en_matriz matriz))
    )



numero_cuatro_2_seguidos_en_matriz :: [[Int]] -> Int
numero_cuatro_2_seguidos_en_matriz matriz =
    (
    (hay_cuatro_2_seguidos matriz) +
    (hay_cuatro_2_seguidos (transpuesta(matriz))) +
    (hay_cuatro_2_seguidos (diagonales_principales(matriz))) + 
    (hay_cuatro_2_seguidos (diagonales_secundarias(matriz)))
    )
    
hay_cuatro_2_seguidos :: [[Int]] -> Int
hay_cuatro_2_seguidos matriz = empezar_fila_i_numero_2_seguidos matriz 0

numero_cuatro_2_seguidos_en_fila :: [Int] -> Int
numero_cuatro_2_seguidos_en_fila (w:x:y:z:zs)
    |w == x && x == y && y == z && z == 2 = 1 + (numero_cuatro_2_seguidos_en_fila (x:y:z:zs))
    |otherwise = (numero_cuatro_2_seguidos_en_fila (x:y:z:zs))
numero_cuatro_2_seguidos_en_fila _ = 0

empezar_fila_i_numero_2_seguidos :: [[Int]] -> Int -> Int
empezar_fila_i_numero_2_seguidos matriz i
    |i < length(matriz) = (numero_cuatro_2_seguidos_en_fila (matriz!!(i))) + (empezar_fila_i_numero_2_seguidos matriz (i+1))
    |otherwise = 0
    
    
    
numero_tres_2_seguidos_en_matriz :: [[Int]] -> Int
numero_tres_2_seguidos_en_matriz matriz =
    (
    (hay_tres_2_seguidos matriz) +
    (hay_tres_2_seguidos (transpuesta(matriz))) +
    (hay_tres_2_seguidos (diagonales_principales(matriz))) + 
    (hay_tres_2_seguidos (diagonales_secundarias(matriz)))
    )

hay_tres_2_seguidos :: [[Int]] -> Int
hay_tres_2_seguidos matriz = empezar_fila_i_numero_2_seguidos3 matriz 0
    
numero_tres_2_seguidos_en_fila :: [Int] -> Int
numero_tres_2_seguidos_en_fila (w:x:y:z:zs)
    |w == x && x == y && y == 2 && z == 0 = 1 + (numero_tres_2_seguidos_en_fila (x:y:z:zs))
    |w == 0 && x == y && y == z && z == 2 = 1 + (numero_tres_2_seguidos_en_fila (x:y:z:zs))
    |otherwise = (numero_tres_2_seguidos_en_fila (x:y:z:zs))
numero_tres_2_seguidos_en_fila _ = 0

empezar_fila_i_numero_2_seguidos3 :: [[Int]] -> Int -> Int
empezar_fila_i_numero_2_seguidos3 matriz i
    |i < length(matriz) = (numero_tres_2_seguidos_en_fila (matriz!!(i))) + (empezar_fila_i_numero_2_seguidos3 matriz (i+1))
    |otherwise = 0


numero_dos_2_seguidos_en_matriz :: [[Int]] -> Int
numero_dos_2_seguidos_en_matriz matriz =
    (
    (hay_dos_2_seguidos matriz) +
    (hay_dos_2_seguidos (transpuesta(matriz))) +
    (hay_dos_2_seguidos (diagonales_principales(matriz))) + 
    (hay_dos_2_seguidos (diagonales_secundarias(matriz)))
    )

hay_dos_2_seguidos :: [[Int]] -> Int
hay_dos_2_seguidos matriz = empezar_fila_i_numero_2_seguidos2 matriz 0

numero_dos_2_seguidos_en_fila :: [Int] -> Int
numero_dos_2_seguidos_en_fila (x:y:z:zs)
    |x == y && y == 2 && z == 0 = 1 + (numero_dos_2_seguidos_en_fila (y:z:zs))
    |x == 0 && y == z && z == 2 = 1 + (numero_dos_2_seguidos_en_fila (y:z:zs))
    |otherwise = (numero_dos_2_seguidos_en_fila (y:z:zs))
numero_dos_2_seguidos_en_fila _ = 0

empezar_fila_i_numero_2_seguidos2 :: [[Int]] -> Int -> Int
empezar_fila_i_numero_2_seguidos2 matriz i
    |i < length(matriz) = (numero_dos_2_seguidos_en_fila (matriz!!(i))) + (empezar_fila_i_numero_2_seguidos2 matriz (i+1))
    |otherwise = 0
    

numero_uno_2_seguidos_en_matriz :: [[Int]] -> Int
numero_uno_2_seguidos_en_matriz matriz =
    (
    (hay_uno_2_seguidos matriz) +
    (hay_uno_2_seguidos (transpuesta(matriz))) +
    (hay_uno_2_seguidos (diagonales_principales(matriz))) + 
    (hay_uno_2_seguidos (diagonales_secundarias(matriz)))
    )

hay_uno_2_seguidos :: [[Int]] -> Int
hay_uno_2_seguidos matriz = empezar_fila_i_numero_2_seguidos1 matriz 0

numero_uno_2_seguidos_en_fila :: [Int] -> Int
numero_uno_2_seguidos_en_fila (x:y:ys)
    |x == 0 && y == 2 = 1 + (numero_uno_2_seguidos_en_fila (y:ys))
    |x == 2 && y == 0 = 1 + (numero_uno_2_seguidos_en_fila (y:ys))
    |otherwise = (numero_uno_2_seguidos_en_fila (y:ys))
numero_uno_2_seguidos_en_fila _ = 0

empezar_fila_i_numero_2_seguidos1 :: [[Int]] -> Int -> Int
empezar_fila_i_numero_2_seguidos1 matriz i
    |i < length(matriz) = (numero_uno_2_seguidos_en_fila (matriz!!(i))) + (empezar_fila_i_numero_2_seguidos1 matriz (i+1))
    |otherwise = 0
    

numero_cuatro_1_seguidos_en_matriz :: [[Int]] -> Int
numero_cuatro_1_seguidos_en_matriz matriz =
    (
    (hay_cuatro_1_seguidos matriz) +
    (hay_cuatro_1_seguidos (transpuesta(matriz))) +
    (hay_cuatro_1_seguidos (diagonales_principales(matriz))) + 
    (hay_cuatro_1_seguidos (diagonales_secundarias(matriz)))
    )
    
hay_cuatro_1_seguidos :: [[Int]] -> Int
hay_cuatro_1_seguidos matriz = empezar_fila_i_numero_1_seguidos matriz 0

numero_cuatro_1_seguidos_en_fila :: [Int] -> Int
numero_cuatro_1_seguidos_en_fila (w:x:y:z:zs)
    |w == x && x == y && y == z && z == 1 = 1 + (numero_cuatro_1_seguidos_en_fila (x:y:z:zs))
    |otherwise = (numero_cuatro_1_seguidos_en_fila (x:y:z:zs))
numero_cuatro_1_seguidos_en_fila _ = 0

empezar_fila_i_numero_1_seguidos :: [[Int]] -> Int -> Int
empezar_fila_i_numero_1_seguidos matriz i
    |i < length(matriz) = (numero_cuatro_1_seguidos_en_fila (matriz!!(i))) + (empezar_fila_i_numero_1_seguidos matriz (i+1))
    |otherwise = 0
    
    
    
numero_tres_1_seguidos_en_matriz :: [[Int]] -> Int
numero_tres_1_seguidos_en_matriz matriz =
    (
    (hay_tres_1_seguidos matriz) +
    (hay_tres_1_seguidos (transpuesta(matriz))) +
    (hay_tres_1_seguidos (diagonales_principales(matriz))) + 
    (hay_tres_1_seguidos (diagonales_secundarias(matriz)))
    )

hay_tres_1_seguidos :: [[Int]] -> Int
hay_tres_1_seguidos matriz = empezar_fila_i_numero_1_seguidos3 matriz 0
    
numero_tres_1_seguidos_en_fila :: [Int] -> Int
numero_tres_1_seguidos_en_fila (w:x:y:z:zs)
    |w == x && x == y && y == 1 && z == 0 = 1 + (numero_tres_1_seguidos_en_fila (x:y:z:zs))
    |w == 0 && x == y && y == z && z == 1 = 1 + (numero_tres_1_seguidos_en_fila (x:y:z:zs))
    |otherwise = (numero_tres_1_seguidos_en_fila (x:y:z:zs))
numero_tres_1_seguidos_en_fila _ = 0

empezar_fila_i_numero_1_seguidos3 :: [[Int]] -> Int -> Int
empezar_fila_i_numero_1_seguidos3 matriz i
    |i < length(matriz) = (numero_tres_1_seguidos_en_fila (matriz!!(i))) + (empezar_fila_i_numero_1_seguidos3 matriz (i+1))
    |otherwise = 0


numero_dos_1_seguidos_en_matriz :: [[Int]] -> Int
numero_dos_1_seguidos_en_matriz matriz =
    (
    (hay_dos_1_seguidos matriz) +
    (hay_dos_1_seguidos (transpuesta(matriz))) +
    (hay_dos_1_seguidos (diagonales_principales(matriz))) + 
    (hay_dos_1_seguidos (diagonales_secundarias(matriz)))
    )

hay_dos_1_seguidos :: [[Int]] -> Int
hay_dos_1_seguidos matriz = empezar_fila_i_numero_1_seguidos2 matriz 0

numero_dos_1_seguidos_en_fila :: [Int] -> Int
numero_dos_1_seguidos_en_fila (x:y:z:zs)
    |x == y && y == 1 && z == 0 = 1 + (numero_dos_1_seguidos_en_fila (y:z:zs))
    |x == 0 && y == z && z == 1 = 1 + (numero_dos_1_seguidos_en_fila (y:z:zs))
    |otherwise = (numero_dos_1_seguidos_en_fila (y:z:zs))
numero_dos_1_seguidos_en_fila _ = 0

empezar_fila_i_numero_1_seguidos2 :: [[Int]] -> Int -> Int
empezar_fila_i_numero_1_seguidos2 matriz i
    |i < length(matriz) = (numero_dos_1_seguidos_en_fila (matriz!!(i))) + (empezar_fila_i_numero_1_seguidos2 matriz (i+1))
    |otherwise = 0
    

numero_uno_1_seguidos_en_matriz :: [[Int]] -> Int
numero_uno_1_seguidos_en_matriz matriz =
    (
    (hay_uno_1_seguidos matriz) +
    (hay_uno_1_seguidos (transpuesta(matriz))) +
    (hay_uno_1_seguidos (diagonales_principales(matriz))) + 
    (hay_uno_1_seguidos (diagonales_secundarias(matriz)))
    )

hay_uno_1_seguidos :: [[Int]] -> Int
hay_uno_1_seguidos matriz = empezar_fila_i_numero_1_seguidos1 matriz 0

numero_uno_1_seguidos_en_fila :: [Int] -> Int
numero_uno_1_seguidos_en_fila (x:y:ys)
    |x == 0 && y == 1  = 1 + (numero_uno_1_seguidos_en_fila (y:ys))
    |x == 1 && y == 0 = 1 + (numero_uno_1_seguidos_en_fila (y:ys))
    |otherwise = (numero_uno_1_seguidos_en_fila (y:ys))
numero_uno_1_seguidos_en_fila _ = 0

empezar_fila_i_numero_1_seguidos1 :: [[Int]] -> Int -> Int
empezar_fila_i_numero_1_seguidos1 matriz i
    |i < length(matriz) = (numero_uno_1_seguidos_en_fila (matriz!!(i))) + (empezar_fila_i_numero_1_seguidos1 matriz (i+1))
    |otherwise = 0
    



