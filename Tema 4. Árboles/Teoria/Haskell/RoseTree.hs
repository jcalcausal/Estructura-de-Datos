------------------------------------------------------------
-- Estructuras de Datos
-- Tema 4. Árboles
-- Profesor : Pablo López
-- Alumno   : Ángel Manuel Soria Gil
-- Árboles generales (rose trees) en Haskell
------------------------------------------------------------

module RoseTree where

data Tree a = Empty
            | Node a [Tree a]
            deriving Show

gtree1 :: Tree Int
gtree1 =
  Node 1 [ Node 2 [ Node 4 [ ]
                  , Node 5 [ ]
                  , Node 6 [ ]
                  ]
         , Node 3 [ Node 7 [ ] ]
         ]

-- | suma los nodos de un árbol genérico
-- >>> sumT gtree1
-- 28
sumT :: Num a => Tree a -> a
sumT Empty       = 0
sumT (Node a rt) = a + solRes
  where
    solRes = foldr f 0 rt
    f t solAc = solAc + sumT t 

-- | altura (número de niveles) de un árbol genérico
-- >>> heightT gtree1
-- 3
heightT :: Tree a -> Int
heightT Empty       = 0
heightT (Node a []) = 1
heightT (Node a rt) = 1 + hres
  where
    hres = maximum (map heightT rt)

-- | frontera (conjunto de nodos hoja) de un árbol genérico
-- >>> borderT gtree1
-- [4,5,6,7]
borderT :: Tree a -> [a]
borderT Empty = []
borderT (Node a []) = [a]
borderT (Node a rt) = [x | t <- rt, x <- borderT t]

-- | recorrido de un árbol genérico
-- >>> flattenT gtree1
-- [1,2,4,5,6,3,7]
flattenT :: Tree a -> [a]
flattenT Empty = []
flattenT (Node a []) = [a]
flattenT (Node a rt) = [a] ++ [x | t <- rt, x <- flattenT t]
