-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 2
--
-- Alumno: APELLIDOS, NOMBRE
-------------------------------------------------------------------------------

module Practica2 where

import           Data.List       (nub, (\\))
import           Test.QuickCheck

-------------------------------------------------------------------------------
-- Ejercicio 4 - distintos
-------------------------------------------------------------------------------

distintos :: Eq a => [a] -> Bool
distintos = undefined

-------------------------------------------------------------------------------
-- Ejercicio 3 - reparte
-------------------------------------------------------------------------------

reparte :: [a] -> ([a], [a])
reparte = undefined

-------------------------------------------------------------------------------
-- Ejercicio [empareja] de la lista de ejercicios extra
-------------------------------------------------------------------------------

-- Hoogle (https://www.haskell.org/hoogle/) es un buscador para el API de Haskell.
-- Usa Hoogle para encontrar información sobre la función 'zip'.

empareja :: [a] -> [b] -> [(a, b)] -- predefinida como zip
empareja xs ys = undefined

prop_empareja_OK :: (Eq b, Eq a) => [a] -> [b] -> Bool
prop_empareja_OK xs ys = undefined

-------------------------------------------------------------------------------
-- Ejercicio 13 - desconocida
-------------------------------------------------------------------------------

-- Usa Hoogle para consultar la función 'and'.

desconocida :: Ord a => [a] -> Bool
desconocida xs = and [ x <= y | (x, y) <- zip xs (tail xs) ]

-------------------------------------------------------------------------------
-- Ejercicio 14 - inserta y ordena
-------------------------------------------------------------------------------

-- Usa Hoogle para consultar las funciones 'takeWhile' y 'dropWhile'.

-- 14.a - usando takeWhile y dropWhile
inserta :: untyped
inserta = undefined

-- 14.b - mediante recursividad
insertaRec :: untyped
insertaRec = undefined

-- 14.c

-- Las líneas de abajo no compilan hasta que completes los apartados
-- anteriores. Cuando los completes, elimina los guiones del comentario
-- y comprueba tus funciones.

-- prop_inserta :: Ord a => a -> [a] -> Property
-- prop_inserta x xs = desconocida xs ==> desconocida (inserta x xs)

-- prop_insertaRec :: Ord a => a -> [a] -> Property
-- prop_insertaRec x xs = desconocida xs ==> desconocida (insertaRec x xs)

-- 14.e - usando foldr
ordena :: untyped
ordena = undefined

-- Para definir prop_ordena_OK tendrás que usar el operador sobre listas (\\).
-- Consulta Hoogle.

-- 14.f
prop_ordena_OK :: untyped
prop_ordena_OK = undefined

-------------------------------------------------------------------------------
-- Ejercicio [mezcla] de la lista de ejercicios extra
-------------------------------------------------------------------------------

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla  ys = undefined

-------------------------------------------------------------------------------
-- Ejercicio [pares] de la lista de ejercicios extra
-------------------------------------------------------------------------------

cotizacion :: [(String, Double)]
cotizacion = [("apple", 116), ("intel", 35), ("google", 824), ("nvidia", 67)]

buscarRec :: Eq a => a -> [(a,b)] -> [b]
buscarRec x ys = undefined

buscarC :: Eq a => a -> [(a, b)] -> [b]
buscarC x ys = undefined

buscarP :: Eq a => a -> [(a, b)] -> [b]
buscarP x ys = undefined

prop_buscar_OK :: (Eq a, Eq b) => a -> [(a, b)] -> Bool
prop_buscar_OK x ys = undefined

{-

Responde las siguientes preguntas si falla la propiedad anterior.

¿Por qué falla la propiedad prop_buscar_OK?

Realiza las modificaciones necesarias para que se verifique la propiedad.

-}

valorCartera :: [(String, Double)] -> [(String, Double)] -> Double
valorCartera cartera mercado = undefined

-------------------------------------------------------------------------------
-- Ejercicio 12 - concat
-------------------------------------------------------------------------------

concatP :: [[a]] -> [a]
concatP xss = undefined

concatC :: [[a]] -> [a]
concatC xss = undefined
