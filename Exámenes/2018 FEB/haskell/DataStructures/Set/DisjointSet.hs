-------------------------------------------------------------------------------
-- Student's name: Ángel Manuel Soria Gil
-- Student's group: Doble Grado Matemáticas e Ingeniería Informática D
--
-- Data Structures. February 2018. BSc. Computer Science. UMA.
-------------------------------------------------------------------------------

module DataStructures.Set.DisjointSet
                  ( DisjointSet
                  , empty
                  , isEmpty
                  , isElem
                  , numElements
                  , add
                  , areConnected
                  , kind
                  , union
                  , flatten
                  , kinds
                  ) where

import           Data.List                               (intercalate)
import           Data.Maybe                              (fromJust)
import qualified DataStructures.Dictionary.AVLDictionary as D

data DisjointSet a = DS (D.Dictionary a a)

-- | Exercise 1. empty

empty :: DisjointSet a
empty = DS (D.empty)

-- | Exercise 2.a isEmpty

isEmpty :: DisjointSet a -> Bool
isEmpty (DS d) = D.isEmpty d 

-- | Exercise 2.b isElem

isElem :: (Ord a) => a -> DisjointSet a -> Bool
isElem  x (DS dict) = D.isDefinedAt x dict

-- | Exercise 3. numElements

numElements :: DisjointSet a -> Int
numElements (DS dict) = length (D.keys dict)

-- | Exercise 4. add

add :: Ord a => a -> DisjointSet a -> DisjointSet a
add elem (DS dict) 
  | D.isDefinedAt elem dict = DS dict
  | otherwise               = DS (D.insert elem elem dict) 
 
-- | Exercise 5. root

root :: Ord a => a -> DisjointSet a -> Maybe a
root elem (DS dict) 
  | not (D.isDefinedAt elem dict)    = Nothing
  | D.valueOf elem dict == Just elem = Just elem
  | otherwise                        = root k (DS dict)
  where
    k = case D.valueOf elem dict of
      Nothing -> elem
      Just v  -> v

rootAux :: Ord a => a -> DisjointSet a -> a 
rootAux elem (DS dict)
  | not (D.isDefinedAt elem dict)    = error "error: you shouldn't have used this here"
  | D.valueOf elem dict == Just elem = elem
  | otherwise                        = rootAux k (DS dict)
  where
    k = case D.valueOf elem dict of
      Nothing -> elem
      Just v  -> v
  

-- | Exercise 6. isRoot

isRoot :: Ord a => a -> DisjointSet a -> Bool
isRoot elem (DS dict) = D.valueOf elem dict == Just elem

-- | Exercise 7. areConnected

areConnected :: Ord a => a -> a -> DisjointSet a -> Bool
areConnected x y djs = root x djs == root y djs

-- | Exercise 8. kind

kind :: Ord a => a -> DisjointSet a -> [a]
kind x (DS dict) = [y | y <- D.keys dict, areConnected x y (DS dict)]

-- | Exercise 9. union

union :: Ord a => a -> a -> DisjointSet a -> DisjointSet a
union x y (DS dict)
  | x == y    = (DS dict)
  | otherwise = (DS modifiedDict)
  where
    modifiedDict = D.insert highRoot smallRoot dict
    highRoot     = max (rootAux x (DS dict)) (rootAux y (DS dict))
    smallRoot    = min (rootAux x (DS dict)) (rootAux y (DS dict))
        

-- |------------------------------------------------------------------------

flatten :: Ord a => DisjointSet a -> DisjointSet a
flatten (DS dict) = (DS (insertList list D.empty))
  where
    list = [(x,r) | x <- D.keys dict, let r = rootAux x (DS dict)]

insertList :: Ord a => [(a,a)] -> D.Dictionary a a -> D.Dictionary a a
insertList [] solAc           = solAc
insertList ((k,v):rest) solAc = insertList rest (D.insert k v solAc)


kinds :: Ord a => DisjointSet a -> [[a]]
kinds (DS dict) = filter (onlyOnce list) list
  where 
    list = [v | x <- D.keys dict, let v = kind x (DS dict)]

onlyOnce :: Ord a => [a] -> a -> Bool
onlyOnce list elem = appearences elem list == 1

appearences :: (Ord a) => a -> [a] -> Int
appearences x list = foldr f 0 list
  where
    f elem solAc
      | elem == x = solAc + 1
      | otherwise = solAc

-- |------------------------------------------------------------------------

instance (Ord a, Show a) => Show (DisjointSet a) where
  show (DS d)  = "DictionaryDisjointSet(" ++ intercalate "," (map show (D.keysValues d)) ++ ")"


{-

-- Examples

-- | Exercise 1. empty

>>> empty
DictionaryDisjointSet()

-- | Exercise 2.a isEmpty

>>> isEmpty empty
True

>>> isEmpty (add 1 empty)
False

-- | Exercise 2.b isElem

>>> isElem 1 empty
False

>>> isElem 1 (add 1 empty)
True

>>> isElem 2 (add 1 empty)
False

>>> isElem 1 (add 2 (add 1 empty))
True

-- | Exercise 3. numElements

>>> numElements empty
0

>>> numElements (add 1 empty)
1

>>> numElements (add 2 (add 1 empty))
2

-- | Exercise 4. add

>>> add 1 empty
DictionaryDisjointSet((1,1))

>>> add 2 (add 1 empty)
DictionaryDisjointSet((1,1),(2,2))

>>> add 1 (add 2 (add 1 empty))
DictionaryDisjointSet((1,1),(2,2))

-- | Exercise 5. root

>>> root 1 empty
Nothing

>>> root 1 (add 1 empty)
Just 1

>>> root 2 (add 2 (add 1 empty))
Just 2

>>> root 1 (union 1 2 (add 2 (add 1 empty)))
Just 1

>>> root 2 (union 1 2 (add 2 (add 1 empty)))
Just 1

>>> root 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
Just 1

>>> root 2 (union 1 3 (add 3 (add 2 (add 1 empty))))
Just 2

>>> root 3 (union 1 3 (add 3 (add 2 (add 1 empty))))
Just 1

>>> root 4 (union 1 3 (add 3 (add 2 (add 1 empty))))
Nothing

-- | Exercise 6. isRoot

>>> isRoot 1 empty
False

>>> isRoot 1 (add 1 empty)
True

>>> isRoot 1 (union 1 2 (add 2 (add 1 empty)))
True

>>> isRoot 2 (union 1 2 (add 2 (add 1 empty)))
False

>>> isRoot 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> isRoot 2 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> isRoot 3 (union 1 3 (add 3 (add 2 (add 1 empty))))
False

-- | Exercise 7. areConnected

>>> areConnected 1 3 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> areConnected 3 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> areConnected 1 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> areConnected 1 2 (union 1 3 (add 3 (add 2 (add 1 empty))))
False

>>> areConnected 1 2 (union 2 3 (union 1 3 (add 3 (add 2 (add 1 empty)))))
True

>>> areConnected 1 5 (union 2 3 (union 1 3 (add 3 (add 2 (add 1 empty)))))
False

-- | Exercise 8. kind

>>> kind 1 (add 2 (add 1 empty))
[1]

>>> kind 2 (add 2 (add 1 empty))
[2]

>>> kind 3 (add 2 (add 1 empty))
[]

>>> kind 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
[1,3]

>>> kind 3 (union 1 3 (add 3 (add 2 (add 1 empty))))
[1,3]

>>> kind 2 (union 1 3 (add 3 (add 2 (add 1 empty))))
[2]

>>> kind 2 (union 2 3 (union 1 3 (add 3 (add 2 (add 1 empty)))))
[1,2,3]

-- | Exercise 9. union

>>> union 1 2 (add 2 (add 1 empty))
DictionaryDisjointSet((1,1),(2,1))

>>> union 2 1 (add 2 (add 1 empty))
DictionaryDisjointSet((1,1),(2,1))

>>> union 1 1 (add 2 (add 1 empty))
DictionaryDisjointSet((1,1),(2,2))

>>> union 1 3 (add 3 (add 2 (add 1 empty)))
DictionaryDisjointSet((1,1),(2,2),(3,1))

>>> union 1 2 (add 1 empty)
*** Exception: union: missing element(s)

-}
