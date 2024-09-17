-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- PRACTICA 6. Montículos maxifóbicos en Haskel
--
-- Alumno: Ángel Manuel Soria Gil
-------------------------------------------------------------------------------

-- Solo tienes que completar el código de la función 'merge' de acuerdo con
-- las explicaciones del enunciado.

module MaxiphobicHeap
  ( Heap
  , empty
  , isEmpty
  , minElem
  , delMin
  , insert
  , merge
-- los siguientes son auxiliares
  , mkHeap
  , verifyOP
  , drawOnWith
  ) where

import           DataStructures.Graphics.DrawTrees
import           Test.QuickCheck

data Heap a = Empty | Node a Int (Heap a) (Heap a) deriving Show

size :: Heap a -> Int
size Empty           = 0
size (Node _ sz _ _) = sz

empty :: Heap a
empty  = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

singleton :: a -> Heap a
singleton x  = Node x 1 Empty Empty

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h  = merge h (singleton x)

minElem :: Heap a -> a
minElem Empty          = error "minElem on empty heap"
minElem (Node x _ _ _) = x

delMin :: (Ord a) => Heap a -> Heap a
delMin Empty            = error "delMin on empty heap"
delMin (Node _ _ lh rh) = merge lh rh

----------------------------------------------------------
-- VVVVVVVVVVVV-SOLO TOCAR ABAJO-VVVVVVVVVVVV-------------
----------------------------------------------------------

{-
    Al mezclar tenemos tres montículos: el ganador y los hijos del perdedor.
    Se cuelga a la izquierda el más pesado de los tres y a la derecha la mezcla
    de los más ligeros:

                           mínimo
                          /      \
                       más       mezcla de los
                     pesado       más ligeros

   Política de desempate:

   Si hay empate para elegir el montículo más pesado:

    1. elegir preferentemente al hijo izquierdo del perdedor
    2. si no, elegir preferentemente al hijo derecho del perdedor
    3. si no, elegir al ganador
-}

-- recursively merges smallest subheaps. Achieves O(log n) complexity
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge m1 Empty = m1
merge Empty m1 = m1
merge h1@(Node x1 s1 lt1 rt1) h2@(Node x2 s2 lt2 rt2) -- El @ se usa para ponerle un nombre a cada árbol con el que referirnos
  | x1 <= x2  = Node x1 (s1+s2) masPesado1 (merge o1 o2)
  | otherwise = Node x2 (s1+s2) masPesado2 (merge o3 o4)
  where 
    (masPesado1, o1, o2) = ordena h2 lt1 rt1
    (masPesado2, o3, o4) = ordena h1 lt2 rt2
    ordena w i d
      | size w > size i && size w > size d = (w,i,d)
      | size i >= size w && size i >= size d = (i,w,d)
      | size d >= size w && size d > size i = (d,w,i)
    
 
----------------------------------------------------------
-- ^^^^^^^^^^^^^^-- SOLO TOCAR ARRIBA ^^^^^^^^^^^ --------
----------------------------------------------------------

-- Efficient O(n) bottom-up construction for heaps
mkHeap :: (Ord a) => [a] -> Heap a
mkHeap []  = empty
mkHeap xs  = mergeLoop (map singleton xs)
  where
    mergeLoop [h] = h
    mergeLoop hs  = mergeLoop (mergePairs hs)

    mergePairs []        = []
    mergePairs [h]       = [h]
    mergePairs (h:h':hs) = merge h h' : mergePairs hs

-------------------------------------------------------------------------------
-- Generating arbitrary Heaps
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary  = do
    xs <- arbitrary
    return (mkHeap xs)

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

verifyOP :: (Ord a) => Heap a -> Bool
verifyOP Empty             = True
verifyOP (Node x _ lh rh)  = x `lessEq` lh && x `lessEq` rh
                           && verifyOP lh && verifyOP rh
 where
  x `lessEq` Empty           = True
  x `lessEq` (Node x' _ _ _) = x<=x'

-------------------------------------------------------------------------------
-- Drawing a Heap
-------------------------------------------------------------------------------

instance Subtrees (Heap a) where
  subtrees Empty            = []
  subtrees (Node _ _ lh rh) = [lh,rh]

  isEmptyTree Empty = True
  isEmptyTree _     = False

instance (Show a) => ShowNode (Heap a) where
  showNode (Node x _ _ _) = show x

drawOnWith :: FilePath -> (a -> String) -> Heap a -> IO ()
drawOnWith file toString = _drawOnWith file showHeap
 where
  showHeap (Node x _ _ _) = toString x
