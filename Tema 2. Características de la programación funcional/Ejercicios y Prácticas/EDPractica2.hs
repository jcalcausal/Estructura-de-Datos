-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 2
--
-- Alumno: Ángel Manuel Soria Gil
-------------------------------------------------------------------------------

module Practica2 where

import           Data.List       (nub, (\\))
import           Test.QuickCheck

instance Show (a -> b) where
         show a = "funcion"

-------------------------------------------------------------------------------
-- Ejercicio 3 - reparte
-------------------------------------------------------------------------------
reparte :: [a] -> ([a], [a])
reparte lista = reparteAux lista 0 ([], [])
    where
        reparteAux [] _ solAc = solAc
        reparteAux (x:xs) ac (pars, imps)
            | even ac = reparteAux xs (ac+1) (pars++[x], imps)
            | otherwise = reparteAux xs (ac+1) (pars, imps++[x])

-------------------------------------------------------------------------------
-- Ejercicio 4 - distintos
-------------------------------------------------------------------------------
distintos :: Eq a => [a] -> Bool
distintos [] = True
distintos (x:xs) =  (not (x `elem` xs)) && distintos xs

-------------------------------------------------------------------------------
-- Ejercicio 12 - concat
-------------------------------------------------------------------------------
concatP :: [[a]] -> [a]
concatP xss = foldr (++) [] xss

concatC :: [[a]] -> [a]
concatC xss = [x | xs <- xss, x <- xs]


-------------------------------------------------------------------------------
-- Ejercicio 13 - desconocida
-------------------------------------------------------------------------------
-- Usa Hoogle para consultar la función 'and'.
desconocida :: Ord a => [a] -> Bool
desconocida xs = and [ x <= y | (x, y) <- zip xs (tail xs) ]

-- Se recibe una lista de valores ordenables. Se devuelve un Bool. Dicho valor Bool es la conjunción lógica de valores Bool de otra lista.
-- Esta otra lista es una lista de Bool dados por x <= y donde (x,y) son los pares que devuelve la función zip xs (tail xs), esto es,
-- si tengo una lista  xs = [1,2,3,4,5,6,7,8,9], los pares (x,y) serían (1,2), (2,3) ... (8, 9), vaya, que al fin y al cabo devuelve 
-- si en una lista [a1, ... ai, ai+1, ... an], ai<=ai+i para todo i entre 1 y n-1, es decir, si la lista está ordenada.

-------------------------------------------------------------------------------
-- Ejercicio 14 - inserta y ordena
-------------------------------------------------------------------------------
-- Usa Hoogle para consultar las funciones 'takeWhile' y 'dropWhile'.
-- 14.a - usando takeWhile y dropWhile
inserta :: Ord a => a -> [a] -> [a]
inserta x xs = takeWhile (<x) xs ++ [x] ++ dropWhile (<x) xs

-- 14.b - mediante recursividad
insertaRec :: Ord a => a -> [a] -> [a]
insertaRec x [] = [x]
insertaRec x (y:ys)
    | x <= y = (x:(y:ys))
    |otherwise = (y : insertaRec x ys) 

-- 14.c
-- Las líneas de abajo no compilan hasta que completes los apartados
-- anteriores. Cuando los completes, elimina los guiones del comentario
-- y comprueba tus funciones.

prop_inserta :: Ord a => a -> [a] -> Property
prop_inserta x xs = desconocida xs ==> desconocida (inserta x xs)

prop_insertaRec :: Ord a => a -> [a] -> Property
prop_insertaRec x xs = desconocida xs ==> desconocida (insertaRec x xs)

-- 14.d 
-- Se puede utilizar inserta para ordenar una lista de valores puesto que si comienzas insertando un valor a la lista vacía, consigues una lista de un elemento ordenada
-- y la función inserta lo hace manteniendo el orden, así a partir de una lista desordenada podemos decirle que inserte sus valores en una lista vacía y la devolverá ordenada.

-- 14.e - usando foldr
ordena :: Ord a => [a] -> [a]
ordena xs = foldr inserta [] xs

-- Para definir prop_ordena_OK tendrás que usar el operador sobre listas (\\).
-- Consulta Hoogle.

-- 14.f
prop_ordena_OK :: Ord a => [a] -> Property
prop_ordena_OK xs = ordena xs \\ xs == [] ==> (desconocida (ordena xs))

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIOS RELACIÓN EXTRA (PRÁCTICA II)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Ejercicio [empareja] de la lista de ejercicios extra
-------------------------------------------------------------------------------
-- Hoogle (https://www.haskell.org/hoogle/) es un buscador para el API de Haskell.
-- Usa Hoogle para encontrar información sobre la función 'zip'.

empareja :: [a] -> [b] -> [(a, b)] -- predefinida como zip
empareja [] _ = []
empareja _ [] = [] 
empareja (x:xs) (y:ys) = [(x,y)] ++ empareja xs ys

prop_empareja_OK :: (Eq b, Eq a) => [a] -> [b] -> Bool
prop_empareja_OK xs ys = empareja xs ys == zip xs ys

-------------------------------------------------------------------------------
-- Ejercicio. Empareja con.
-------------------------------------------------------------------------------
emparejaCon :: (a -> b -> c) -> [a] -> [b] -> [c]
emparejaCon _ [] _ = []
emparejaCon _ _ [] = []
emparejaCon f (x:xs) (y:ys) = (x `f` y:emparejaCon f xs ys) 

prop_emparejaCon_OK :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> [a] -> [b] -> Bool
prop_emparejaCon_OK f xs ys = emparejaCon f xs ys == zipWith f xs ys

-----------------------------------------------------------------------------------
-- Ejercicio. Separa.
-----------------------------------------------------------------------------------
separaRec :: (a -> Bool) -> [a] -> ([a], [a])
separaRec f lista = separaRecAux f lista [] []
    where 
        separaRecAux f [] solSi solNo = (solSi, solNo)
        separaRecAux f (x:xs) solSi solNo
            | f x = separaRecAux f xs (solSi ++ [x]) solNo
            | otherwise = separaRecAux f xs solSi (solNo ++ [x])

separaP :: (a -> Bool) -> [a] -> ([a], [a])
separaP f lista = foldr aux ([], []) lista 
    where 
        aux elem (solAcSi, solAcNo)
            | f elem = ([elem]++solAcSi, solAcNo)
            | otherwise = (solAcSi, [elem]++solAcNo)

separaC :: (a -> Bool) -> [a] -> ([a], [a])
separaC f lista = ([x | x <- lista, f x], [x | x <- lista, not (f x)])

prop_Separas_Equivalente :: (Show a, Eq a) => (a -> Bool) -> [a] -> Bool
prop_Separas_Equivalente f lista =  (separaC f lista == separaP f lista) && (separaP f lista == separaRec f lista)


-------------------------------------------------------------------------------
-- Ejercicio [pares] de la lista de ejercicios extra
-------------------------------------------------------------------------------
cotizacion :: [(String, Double)]
cotizacion = [("apple", 116), ("intel", 35), ("google", 824), ("nvidia", 67), ("google", 456)]

buscarRec :: Eq a => a -> [(a,b)] -> [b]
buscarRec x [] = []
buscarRec x (y:restoLista)
    | x == k = [v] 
    | otherwise = buscarRec x restoLista
        where (k,v) = y

buscarC :: Eq a => a -> [(a, b)] -> [b]
buscarC _ [] = []
buscarC  val lista = [head [v | (x, v) <- lista, x == val ]]

buscarP :: Eq a => a -> [(a, b)] -> [b]
buscarP x ys = foldr aux [] ys
    where 
        aux (k, v) oth 
            | x == k = [v]
            | otherwise = oth 

prop_buscar_OK :: (Eq a, Eq b) => a -> [(a, b)] -> Bool
prop_buscar_OK x ys = buscarRec x ys == buscarC x ys && buscarC x ys == buscarP x ys

{-
Responde las siguientes preguntas si falla la propiedad anterior.
¿Por qué falla la propiedad prop_buscar_OK?
Realiza las modificaciones necesarias para que se verifique la propiedad.

Me fallaba porque en buscarC tenía problemas con lo de la lista vacía, en el resto devolvía una lista vacía 
pero en el de buscarC devolvía una excepción
-}

valorCartera :: [(String, Double)] -> [(String, Double)] -> Double
valorCartera cartera mercado = foldr aux 0 cartera
    where 
        aux (t, c) valorAc = valorAc + val 
            where 
                val =  (head (buscarC t mercado))*(head (buscarC t cartera))


-------------------------------------------------------------------------------
-- Ejercicio [mezcla] de la lista de ejercicios extra
-------------------------------------------------------------------------------
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla  xs ys = ordena (xs++ys)

----------------------------------------------------------------------------------
-- Ejercicio takeUntil
----------------------------------------------------------------------------------
takeUntil :: (a -> Bool) -> [a]-> [a]
takeUntil _ [] = []
takeUntil f (x:xs) = takeUntilAux f (x:xs) []
    where
        takeUntilAux f (x:xs) solAc
            |f x = solAc
            |otherwise = takeUntilAux f xs (solAc++[x])

prop_takeUntil_OK :: (Eq a) => (a -> Bool) -> [a] -> Bool
prop_takeUntil_OK f xs = takeUntil f xs == takeWhile aux xs
    where aux x = not (f x)

-- Creo que el razonamiento es adecuado, pero no me funciona al pasar el quickCheck 
-- por tema de tipos, si se pasase con tipos y funciones con los tipos correctos supongo
-- que si que funcionaría.

------------------------------------------------------------------------------------------
-- Ejercicio Numeros Felices.
------------------------------------------------------------------------------------------
-- digitosDe
digitosDe :: Integer -> [Integer]
digitosDe n = digitosDeAc n []
    where
        digitosDeAc n solAc
            | n == 0 = solAc
            | otherwise = digitosDeAc (n `div` 10) ((n `mod` 10):solAc)

-- sumaCuadradosDigitos
sumaCuadradosDigitos :: Integer -> Integer
sumaCuadradosDigitos n = sumSqDig (digitosDe n) 
    where 
        sumSqDig xs = foldr aux 0 xs
        aux a b = (a^2) + b

-- esFeliz
esFeliz :: Integer -> Bool
esFeliz n = esFelizAc n [n]
    where 
        esFelizAc n nume
            | sumaCuadradosDigitos n `elem` nume = False
            | sumaCuadradosDigitos n == 1        = True
            | otherwise                          = esFelizAc (sumaCuadradosDigitos n) (n:nume) 

-- felicesHasta
felicesHasta :: Integer -> [Integer]
felicesHasta n = [x | x <- [1..n], esFeliz x]

-- Con length (felicesHasta 1000) nos sale que hay 142 números felices hasta el 1000

----------------------------------------------------------------------------------------------
-- Ejercicio Borrar.
----------------------------------------------------------------------------------------------
borrarRec :: (Eq a) => a -> [a] -> [a]
borrarRec _ [] = []
borrarRec elem (x:xs)
    | elem == x = borrarRec elem xs 
    | otherwise = [x] ++ borrarRec elem xs

borrarP :: (Eq a) => a -> [a] -> [a]
borrarP elem xs = foldr aux [] xs
    where 
        aux el solAc 
            | el == elem = solAc
            | otherwise  = (el:solAc)

borrarC :: (Eq a) => a -> [a] -> [a]
borrarC _ [] = []
borrarC elem xs = [x |x<-xs, x /= elem]

prop_Borrar_OK :: (Eq a) => a -> [a] -> Bool
prop_Borrar_OK x xs = (borrarRec x xs == borrarC x xs) && (borrarC x xs == borrarP x xs)

--------------------------------------------------------------------------------------------
-- Ejercicio Agrupar.
--------------------------------------------------------------------------------------------
agrupar :: (Eq a) => [a] -> [[a]]
agrupar xs = foldr aux [] xs
    where 
        aux elem [] = [[elem]]
        aux elem ((y:ys):resto)
            | elem == y = ((elem:(y:ys)):resto)
            | otherwise = ([elem]:(y:ys):resto)

---------------------------------------------------------------------------------------------
-- Ejercicio aplicar
--------------------------------------------------------------------------------------------- 
aplicarRec :: [(a->b)] -> a -> [b]
aplicarRec [] _ = []
aplicarRec (x:xs) y = [(x y)] ++ aplicarRec xs y 

aplicarM :: [(a->b)] -> a -> [b]
aplicarM [] _ = []
aplicarM xs y = map ($ y) xs    

aplicarP :: [(a->b)] -> a -> [b]
aplicarP xs y = foldr aux [] xs
    where aux f solAc = [f y] ++ solAc

aplicarC :: [(a->b)] -> a -> [b]
aplicarC [] y = []
aplicarC xs y = [z | f <- xs, let z = f y]

prop_aplicar_OK :: (Eq b) => [(a->b)] -> a -> Bool
prop_aplicar_OK xs y = aplicarRec xs y == aplicarC xs y && aplicarC xs y == aplicarM xs y && aplicarM xs y == aplicarP xs y 