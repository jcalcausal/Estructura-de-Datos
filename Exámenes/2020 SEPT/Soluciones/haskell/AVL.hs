{------------------------------------------------------------------------------
 - Student's name: Ángel Manuel Soria Gil
 -
 - Student's group: Doble Grado Matemáticas e Ingeniería Informática.
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
emptyBin cap = B cap []

remainingCapacity :: Bin -> Capacity
remainingCapacity (B cap weights) = cap

addObject :: Weight -> Bin -> Bin
addObject w (B cap weights)
  | cap-w < 0 = error "the object cannot be put on this bin"
  | otherwise = B (cap-w) (weights++[w])

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty = 0
maxRemainingCapacity (Node b h cap avl1 alv2) = cap 

height :: AVL -> Int
height Empty = 0
height (Node b h cap avl1 avl2) = h

nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight (B cap objs) h avl1 avl2 = (Node (B cap objs) h newCap avl1 avl2)
  where 
    newCap = maximum [cap, (maxRemainingCapacity avl1), (maxRemainingCapacity avl2)]

node :: Bin -> AVL -> AVL -> AVL
node (B cap objs) avl1 avl2 = Node (B cap objs) h newCap avl1 avl2
  where
    h = 1 + max (height avl1) (height avl2)
    newCap = maximum [cap, (maxRemainingCapacity avl1), (maxRemainingCapacity avl2)]

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft b lt (Node bin _ _ ls rs) = node bin (node b lt ls) rs 

addNewBin :: Bin -> AVL -> AVL
addNewBin bin Empty = node bin Empty Empty
addNewBin bin (Node b h c avll avlr)
  | height avlr - height avll > 1 = rotateLeft b avll (addNewBin bin avlr)
  | otherwise                     = node b avll (addNewBin bin avlr)
 
addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst c w Empty = addNewBin (B (c-w) [w]) Empty
addFirst c w (Node (B cap objs) ht cp ls rs)
  | c > cp                              = addNewBin (B (c-w) [w]) (Node (B cap objs) ht cp ls rs)
  | maxRemainingCapacity ls >= c        = addFirst c w ls
  | remainingCapacity (B cap objs) >= c = node (B (cap-w) (objs++[w])) ls rs
  | otherwise                           = addFirst c w rs  

addAll:: Capacity -> [Weight] -> AVL
addAll c list = foldr f Empty list
  where 
    f w solAc = addFirst c w solAc

toList :: AVL -> [Bin]
toList Empty                    = []
toList (Node b _ _ Empty Empty) = [b]
toList (Node b h c ls rs)       = toList ls ++ [b] ++ toList rs

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