-- prof: profundidad. Nivel de dificultad.
--       Cuantas jugadas quieres que vaya por delante

-- expandir: es una funcion que dado un tablero
--           te devuelve la lista de posibles tableros,
--           osea los 7 siguientes tableros (por ejemplo)

-- Evaluar: Dado un tablero dice que tan de bueno
--          es para mi (elegimos un valor)

-- problema: Estado actual


minimaxMain prof expandir evaluar problema    
    |null siguientes = Nothing                 
    |otherwise = Just(fst(maximum' sigsVals))   
        where siguientes = expandir problema    
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

expandir :: [[Int]] -> [[[Int]]]



























