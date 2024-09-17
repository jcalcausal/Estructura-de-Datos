-------------------------------------------------------------------------------
-- Estructuras de Datos. UMA
--
-- Implementación del TAD Deque
--
-- Apellidos: Soria Gil
-- Nombre: Ángel Manuel
-- Doble Grado Matemáticas e Ingeniería Informática
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

-- Complexity: O(1)
empty :: DEQue a
empty = DEQ [] []

-- Complexity: O(1)
isEmpty :: DEQue a -> Bool
isEmpty (DEQ [] []) = True
isEmpty _           = False

-- Complexity: O(1)
addFirst :: a -> DEQue a -> DEQue a
addFirst x (DEQ fs ls) = DEQ (x:fs) ls

-- Complexity: O(1)
addLast :: a -> DEQue a -> DEQue a
addLast x (DEQ fs ls) = DEQ fs (x:ls)

-- Complexity: O(1)
first :: DEQue a -> a
first (DEQ fs ls) 
   | length fs == 0 && length ls == 0 = error "first applied on empty list"
   | not (length fs == 0)             = head fs
   | otherwise                        = first (DEQ nfs nls)
      where 
         (nfs, nls) = takeAux ls
         takeAux ls = (reverse (drop m ls), take m ls)
         m          = length ls `div` 2 

-- Complexity: O(1)
last :: DEQue a -> a
last (DEQ fs ls) 
   | length fs == 0 && length ls == 0 = error "last applied on empty list"
   | not (length ls == 0)             = head ls
   | otherwise                        = last (DEQ nfs nls)
      where 
         (nfs, nls) = takeAux fs
         takeAux fs = (take m fs, reverse (drop m fs))
         m          = length fs `div` 2 

-- Complexity: O(1)
deleteFirst :: DEQue a -> DEQue a
deleteFirst (DEQ fs ls) 
   | length fs == 0 && length ls == 0 = empty
   | not (length fs == 0)             = DEQ (tail fs) ls
   | otherwise                        = deleteFirst (DEQ nfs nls)
      where 
         (nfs, nls) = takeAux ls
         takeAux ls = (reverse (drop m ls), take m ls)
         m          = length ls `div` 2 


-- Complexity: O(1)
deleteLast :: DEQue a -> DEQue a
deleteLast (DEQ fs ls) 
   | length fs == 0 && length ls == 0 = empty
   | not (length ls == 0)             = DEQ fs (tail ls)
   | otherwise                        = deleteLast (DEQ nfs nls)
      where 
         (nfs, nls) = takeAux fs
         takeAux fs = (take m fs, reverse (drop m fs))
         m          = length fs `div` 2 



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
