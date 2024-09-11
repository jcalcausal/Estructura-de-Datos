------------------------------------------------------------
-- Estructuras de Datos
-- Tema 4. Árboles
-- Pablo López
--
-- Mezcla de listas ordenadas
------------------------------------------------------------

-- Define una función que mezcle dos listas ordenadas para obtener una lista ordenada.

-- |
-- mezcla
-- >>> mezcla [2,4,6] [ 1, 3, 5]
-- [1,2,3,4,5,6]
--
-- >>> mezcla "adt" "bcz"
--"abcdtz"


-- con patrón alias
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla = undefined
