-------------------------------------------------------------------------------
-- Estructuras de Datos. Grado en Informática, IS e IC. UMA.
-- Examen de Febrero 2015.
--
-- Implementación del TAD Deque
--
-- Apellidos:
-- Nombre:
-- Grado en Ingeniería ...
-- Grupo:
-- Número de PC:
-------------------------------------------------------------------------------

module TwoListsDoubleEndedQueue
   ( DEQue
   , empty
   , isEmpty
   , first
   , last
   , addFirst
   , addLast
   , deleteFirst
   , deleteLast
   ) where

import Prelude hiding (last)
import Data.List(intercalate)
import Test.QuickCheck

data DEQue a = DEQ [a] [a]

-- Complexity:
empty :: DEQue a
empty = DEQ [] []

-- Complexity:
isEmpty :: DEQue a -> Bool
isEmpty (DEQ [] [])  = True
isEmpty _            = False

-- Complexity:
addFirst :: a -> DEQue a -> DEQue a
addFirst x empty             = DEQ [x] []
addFirst x (DEQ list1 list2) = DEQ (x : list1) list2

-- Complexity:
addLast :: a -> DEQue a -> DEQue a
addLast x empty             = DEQ [x][]
addLast x (DEQ list1 list2) = DEQ list1 (x : list2)

-- Complexity:
first :: DEQue a -> a
first empty             = error "first on empty DEQue"
first (DEQ list1 list2) = head list1


-- Complexity:
last :: DEQue a -> a
last empty             = error "last on empty DEQue"
last (DEQ list1 list2) = head list2 

-- Complexity:
deleteFirst :: DEQue a -> DEQue a
deleteFirst empty = empty
deleteFirst (DEQ list1 list2) 
   | null list1 = DEQ newList1 newList2
   | otherwise  = DEQ (tail list1) list2
   where 
      newList1 = (take ((length list2) `div` 2) list2) 
      newList2 = reverse (drop ((length list2) `div` 2) list2)

-- Complexity:
deleteLast :: DEQue a -> DEQue a
deleteLast empty = empty
deleteLast (DEQ list1 list2)
   | null list2 = DEQ newList1 newList2
   | otherwise  = DEQ list1 (tail list2)
   where
      newList1 = (take ((length list1) `div` 2) list1)
      newList2 = reverse (drop ((length list1) `div` 2) list1) 



instance (Show a) => Show (DEQue a) where
   show q = "TwoListsDoubleEndedQueue(" ++ intercalate "," [show x | x <- toList q] ++ ")"

toList :: DEQue a -> [a]
toList (DEQ xs ys) =  xs ++ reverse ys

instance (Eq a) => Eq (DEQue a) where
   q == q' =  toList q == toList q'

instance (Arbitrary a) => Arbitrary (DEQue a) where
   arbitrary =  do
      xs <- listOf arbitrary
      ops <- listOf (oneof [return addFirst, return addLast])
      return (foldr id empty (zipWith ($) ops xs))
