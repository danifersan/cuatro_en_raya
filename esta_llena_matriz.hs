type Matriz = [[Int]]

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



