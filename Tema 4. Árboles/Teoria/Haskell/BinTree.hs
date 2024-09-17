------------------------------------------------------------
-- Estructuras de Datos
-- Tema 4. Árboles
-- Profesor: Pablo López
-- Alumno: Ángel Manuel Soria Gil
-- Árboles binarios en Haskell
------------------------------------------------------------

module BinTree where

import           DataStructures.Graphics.DrawTrees
import           Test.QuickCheck

data TreeB a = EmptyB
             | NodeB a (TreeB a) (TreeB a) deriving Show
               --  root  left     right

{-
   Aunque usamos deriving Show, no es muy útil; estas instancias son
   para pintar árboles binarios en formato gráfico. No es necesario
   saber crear estas instancias, se facilitan cuando sea necesario.
-}

instance Subtrees (TreeB a) where
    isEmptyTree EmptyB = True
    isEmptyTree _      = False

    subtrees EmptyB        = []
    subtrees (NodeB _ i d) = [i,d]

instance Show a => ShowNode (TreeB a) where
    showNode EmptyB        = ""
    showNode (NodeB r _ _) = show r

-- Ejemplos de árboles binarios

tree2 :: TreeB Integer
tree2 =
  NodeB 1 (NodeB 2 (NodeB 4 EmptyB EmptyB)
                   (NodeB 5 EmptyB EmptyB))
          (NodeB 3 (NodeB 6 EmptyB EmptyB)
                    EmptyB)

tree3 :: TreeB Integer
tree3 =
  NodeB 1 (NodeB 2 (NodeB 4 EmptyB EmptyB)
                   (NodeB 5 EmptyB EmptyB))
          (NodeB 3 (NodeB 6 EmptyB EmptyB)
                   (NodeB 2 EmptyB EmptyB))

tree4 :: TreeB Integer
tree4 =
  NodeB 1 (NodeB 2 (NodeB 4 EmptyB EmptyB)
                   (NodeB 5 EmptyB EmptyB))
          (NodeB 3 (NodeB 6 EmptyB EmptyB)
                   (NodeB 7 EmptyB EmptyB))

tree5 :: TreeB Integer
tree5 =
  NodeB 10 (NodeB 20 (NodeB 30 EmptyB EmptyB)
                     (NodeB 30 EmptyB EmptyB))
           (NodeB 30 (NodeB 60 EmptyB EmptyB)
                     (NodeB 20 (NodeB 30 EmptyB EmptyB) EmptyB))

tree6 :: TreeB Char
tree6 = NodeB 'H' (NodeB 'A' (NodeB 'K'
                                       (NodeB 'E' EmptyB EmptyB)
                                       (NodeB 'R' EmptyB EmptyB))
                             (NodeB 'E' EmptyB EmptyB))
                  (NodeB 'S' (NodeB 'L' EmptyB EmptyB)
                             (NodeB 'L' EmptyB EmptyB))

-- Funciones para construir otros árboles binarios de ejemplo

leftChild :: TreeB t -> TreeB t
leftChild (NodeB _ l _) = l

rightChild :: TreeB t -> TreeB t
rightChild (NodeB _ _ r) = r

-- | Ejercicios
------------------------------------------------------------

-- | suma los nodos de un árbol binario
-- >>> sumB tree2
-- 21
sumB :: Num a => TreeB a -> a
sumB EmptyB          = 0
sumB (NodeB a lt rt) = a + (sumB lt) + (sumB rt)

-- | peso (número de nodos) de un árbol binario
-- >>> weightB tree2
-- 6
weightB :: TreeB a -> Int
weightB EmptyB          = 0
weightB (NodeB a lt rt) = 1 + (weightB lt) + (weightB rt) 

-- | altura (número de niveles) de un árbol binario
-- >>> heightB tree2
-- 3
heightB :: (Ord a, Num a) => TreeB t -> a
heightB EmptyB          = 0
heightB (NodeB a lt rt) = 1 + max (heightB lt) (heightB rt)

-- | frontera (conjunto de nodos hoja) de un árbol binario
-- >>> borderB tree2
-- [4,5,6]
borderB :: TreeB a -> [a]
borderB EmptyB                  = []
borderB (NodeB a EmptyB EmptyB) = [a]
borderB (NodeB a lt rt)         = (borderB lt) ++ (borderB rt)

-- | comprobar si 'x' es elemento de un árbol binario
-- >>> isElemB 3 tree2
-- True
-- >>> isElemB 30 tree2
-- False
isElemB :: Eq a => a -> TreeB a -> Bool
isElemB _ EmptyB          = False
isElemB x (NodeB a lt rt) = x==a || isElemB x lt || isElemB x rt

-- | nodos en el nivel 'i' de un árbol binario
-- >>> atLevelB 0 tree2
-- [1]
-- >>> atLevelB 1 tree2
-- [2,3]
-- >>> atLevelB 2 tree2
-- [4,5,6]
-- >>> atLevelB 3 tree2
-- []
atLevelB :: Integer -> TreeB a -> [a]
atLevelB _ EmptyB          = []
atLevelB 0 (NodeB a lt rt) = [a]
atLevelB n (NodeB a lt rt) = (atLevelB (n-1) lt) ++ (atLevelB (n-1) rt) 

-- | caminos hasta el nodo 'x' en un árbol binario
-- >>> pathsToB 5 tree3
-- [[1,2,5]]
-- >>> pathsToB 2 tree3
-- [[1,2],[1,3,2]]
-- >>> pathsToB 9 tree3
-- []
pathsToB :: Eq a => a -> TreeB a -> [[a]]
pathsToB x EmptyB = []
pathsToB x (NodeB a lt rt) 
  | x == a    = ([a] : ps)
  | otherwise = ps
  where 
    ps = map (a:) (pathsToB x lt ++ pathsToB x rt) 


-- | caminos exhaustivos hasta el nodo 'x' en un árbol binario
-- | caminos exhaustivos hasta el nodo 'x' en un árbol binario
-- >>> pathsToB2 30 tree5
-- [[10,20,30],[10,20,30],[10,30],[10,30,20,30]]
pathsToB2 :: (Eq a) => a -> TreeB a -> [[a]]
pathsToB2 x t = pathsToB x t 

-- | recorrido inorden de un árbol binario
-- >>> inOrderB tree4
-- [4,2,5,1,6,3,7]
inOrderB :: TreeB a -> [a]
inOrderB EmptyB          = []
inOrdenB (NodeB a lt rt) = inOrderB lt ++ [a] ++ inOrderB rt  
