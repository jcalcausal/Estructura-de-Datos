{------------------------------------------------------------------------------
 - Student's name: Juan Carlos Alcausa Luque
 -
 - Student's group: -
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
remainingCapacity (B cap we) = cap

addObject :: Weight -> Bin -> Bin
addObject o (B cap we)
    | cap - o < 0 = error "El objeto no cabe en el cubo"
    | otherwise = B (cap - o) (we ++ [o])

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty = 0
maxRemainingCapacity (Node b alt cap lt rt) = cap

height :: AVL -> Int
height Empty = 0
height (Node b alt cap lt rt) = alt


 
nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight (B cap we) h lt rt = (Node (B cap we) h newCap lt rt)
    where
        newCap = maximum [cap, (maxRemainingCapacity lt), (maxRemainingCapacity rt)]


node :: Bin -> AVL -> AVL -> AVL
node (B cap we) lt rt = (Node (B cap we) h newCap lt rt)
    where
        newCap = maximum [cap, (maxRemainingCapacity lt), (maxRemainingCapacity rt)]
        h = 1 + max (height lt) (height rt)

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft c l (Node x _ _ r1 r2) = node x (node c l r1) r2

addNewBin :: Bin -> AVL -> AVL
addNewBin bin Empty = node bin Empty Empty
addNewBin bin (Node b h cap lt rt)
    | height rt - height lt > 1 = rotateLeft b lt (addNewBin bin rt)
    | otherwise = node b lt (addNewBin bin rt)
 
addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst c w Empty = addNewBin (B (c-w) [w]) Empty
addFirst c w nod@(Node x h cp lt rt)
    | w > maxRemainingCapacity nod = addNewBin (B (c-w) [w]) nod
    | w <= maxRemainingCapacity lt = node x (addFirst c w lt) rt
    | w <= remainingCapacity x = node (addObject w x) lt rt
    | otherwise = node x lt (addFirst c w rt)

addAll:: Capacity -> [Weight] -> AVL
addAll c xs = aux c xs Empty
    where
        aux c [] sol = sol
        aux c (x:xs) sol = aux c xs (addFirst c x sol)

toList :: AVL -> [Bin]
toList Empty = []
toList (Node x h cp lt rt) = toList lt ++ [x] ++ toList rt

{-
    SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
 -}

data Sequence = SEmpty | SNode Bin Sequence deriving Show  

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking _ [] = SEmpty
linearBinPacking w (x:xs)
    | x > w = error "El objeto excede la capacidad de los cubos"
    | otherwise = aux (B w []) x (linearBinPacking w xs)
        where
            aux (B w weights) x SEmpty
                | x <= w = SNode (B (w - x) (weights ++ [x])) SEmpty
                | otherwise = SNode (B w weights) (aux (B w []) x SEmpty)
            aux (B w weights) x (SNode b@(B cap weights2) sequence)
                | x <= cap = SNode (B (cap - x) (weights2 ++ [x])) sequence
                | otherwise = SNode b (aux (B w []) x sequence)

seqToList:: Sequence -> [Bin]
seqToList _ = undefined

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold _ _ = undefined



{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"