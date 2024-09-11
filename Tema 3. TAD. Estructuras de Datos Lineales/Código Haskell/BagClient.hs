-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 3 - Uso del TAD Bag
--
-- Alumno: APELLIDOS, NOMBRE
-------------------------------------------------------------------------------

module BagClient where

import           SortedLinearBag

-- `mkBag`: convierte una lista en una bolsa
-- |
-- >>> mkBag "abracadabra"
-- LinearBag { 'a' 'a' 'a' 'a' 'a' 'b' 'b' 'c' 'd' 'r' 'r' }
mkBag :: Ord a => [a] -> Bag a
mkBag xs = foldr insert empty xs

{-
   El TAD Bag es casi inútil sin una función de plegado. El plegado es
   un iterador que permite recorrer una bolsa sin conocer su
   implementación.

   Los siguientes ejercicios se resuelven mediante un plegado de bolsa.
-}

-- `keys`: devuelve una lista con las claves (elementos) que aparecen en la bolsa
-- |
-- >>> keys (mkBag "abracadabra")
-- "abcdr"
keys :: Ord a => Bag a -> [a]
keys bag1 = foldBag f [] bag1
   where  
       f x cx [] = x : []
       f x cx solResto 
                     | x == head solResto = solResto
                     | otherwise =  x : solResto

-- `bag2list`: convierte una bolsa en una lista de pares `(a,Int)`
-- |
-- >>> bag2List (mkBag "abracadabra")
-- [('a',5),('b',2),('c',1),('d',1),('r',2)]
bag2List :: Ord a => Bag a -> [(a, Int)]
bag2List bag1 = foldBag f [] bag1
   where 
      f x cx [] = (x, cx) : []
      f x cx solResto 
         | (x, cx) == head solResto = solResto
         | otherwise =  (x, cx) : solResto

-- `cardinal`: devuelve el número de elementos de una bolsa
-- |
-- >>> cardinal (mkBag "abracadabra")
-- 11
cardinal :: Ord a => Bag a -> Int
cardinal bag1 = foldBag f 0 bag1
   where 
      f x cx solResto = cx + solResto

-- `contains`: devuelve `True` si el dato aparece en la bolsa, `False` en caso contrario
-- |
-- > contains 'a' (mkBag "abracadabra")
-- True
-- > contains 'z' (mkBag "abracadabra")
-- False
contains :: Ord a => a -> Bag a -> Bool
contains x bag1 = foldBag f False bag1
   where
      f y cy solResto = (x == y)||solResto

-- `maxOcurrences`: devuelve el número de veces que aparece el elemento que
-- aparece más veces en una bolsa
-- |
-- >>> maxOcurrences (mkBag "abracadabra")
-- 5
maxOcurrences :: Ord a => Bag a -> Int
maxOcurrences bag1 = foldBag f 0 bag1
   where 
      f x cx solResto = max cx solResto

-- `mostCommons`: devuelve los elementos que aparecen más veces en la bolsa y su cardinal común
-- |
-- >>> mostCommons (mkBag "abcabcab")
-- [('a',3),('b',3)]
mostCommons :: Ord a => Bag a -> [(a,Int)]
mostCommons bag1 = foldBag f [] bag1
   where 
      f x cx solResto 
         | cx > (maxOcurrences bag1)  =  [(x, cx)]
         | cx < (maxOcurrences bag1)  = solResto
         | cx == (maxOcurrences bag1) = (x, cx) : solResto 
