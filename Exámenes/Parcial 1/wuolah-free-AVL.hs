{------------------------------------------------------------------------------
 - Student's name:
 -
 - Student's group:
 -----------------------------------------------------------------------------}

module AVL 
  ( 
    Weight
  , Capacity
  , AVL (..)
  , Bin
  , emptyBin
  , remainingCapacity
  , addObject
  , maxRemainingCapacity
  , height
  , nodeWithHeight
  , node
  , rotateLeft
  , addNewBin
  , addFirst
  , addAll
  , toList
  , linearBinPacking
  , seqToList
  , addAllFold
  ) where

type Capacity = Int
type Weight= Int

data Bin = B Capacity [Weight] 

data AVL = Empty | Node Bin Int Capacity AVL AVL deriving Show


emptyBin :: Capacity -> Bin
emptyBin c = B c []

remainingCapacity :: Bin -> Capacity
remainingCapacity (B c xs) = c

addObject :: Weight -> Bin -> Bin
addObject c d@(B c' xs) |c <= c' = B (c'-c) (c:xs)
                        |otherwise = error "no hay capacidad"

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty = 0
maxRemainingCapacity (Node a h c lt rt) = c 

height :: AVL -> Int
height Empty = 0
height (Node a h c lt rt) = h


 
nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight bin altura ltavl rtavl = Node bin altura (capRestMax ltavl rtavl) ltavl rtavl
   where
    capRestMax lt rt = max (maxRemainingCapacity lt) (maxRemainingCapacity rt)

node :: Bin -> AVL -> AVL -> AVL
node bin ltavl rtavl = Node bin altura (capRestMax ltavl rtavl) ltavl rtavl
 where 
  altura = max (height ltavl) (height rtavl) +1
  capRestMax lt rt = max (maxRemainingCapacity lt) (maxRemainingCapacity rt)

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft c lt (Node bin alt cap llt rt) = node bin (node c lt llt) rt

addNewBin :: Bin -> AVL -> AVL
addNewBin _ _ = undefined
 
addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst w = undefined

addAll:: Capacity -> [Weight] -> AVL
addAll _ _ = undefined

toList :: AVL -> [Bin]
toList (Node bin alt cap lt rt) = toList lt ++ [bin]  ++ toList rt

{-
	SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
 -}

data Sequence = SEmpty | SNode Bin Sequence deriving Show  

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking _ _ = undefined

seqToList:: Sequence -> [Bin]
seqToList _ = undefined

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold _ _ = undefined



{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"