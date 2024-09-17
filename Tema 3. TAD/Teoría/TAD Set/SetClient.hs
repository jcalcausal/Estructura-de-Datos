------------------------------------------------------------
-- Estructuras de Datos
-- Tema 3. Estructuras de Datos Lineales
-- Pablo López
--
-- Módulo cliente del TAD Conjunto
------------------------------------------------------------

module SetClient where

import           Data.Char(toUpper)
import           Set

-- |
-- >>> mkSet [0,2..10]
-- Node 0 (Node 2 (Node 4 (Node 6 (Node 8 (Node 10 Empty)))))
-- mkSet "abracadabra"
-- Node 'a' (Node 'b' (Node 'c' (Node 'd' (Node 'r' Empty))))
mkSet :: Ord a => [a] -> Set a
mkSet xs = foldr insert empty xs

primos :: Set Integer
primos = mkSet [13, 7, 3, 2, 5, 11, 19]

-- |
-- >>> cardinal primos
-- 7
-- >>> cardinal (mkSet "abracadabra")
-- 5
cardinal :: Set a -> Integer
cardinal conjunto = fold f 0 conjunto
    where
        f elem solAc = 1 + solAc

-- |
-- >>> setToList primos
-- [2,3,5,7,11,13,19]
setToList :: Set a -> [a]
setToList conjunto = fold f [] conjunto
    where 
        f elem solAc = (elem:solAc)

-- |
-- >>>  maximo primos
-- Just 19
-- >>> maximo (mkSet "abracadabra")
-- Just 'r'
-- maximo empty
-- Nothing
maximo :: Ord a => Set a -> Maybe a
maximo conjunto = fold f Nothing conjunto
    where 
        f elem (Just solAc)
           | elem > solAc = Just elem
           | otherwise    = Just solAc
        f elem Nothing    = Just elem
