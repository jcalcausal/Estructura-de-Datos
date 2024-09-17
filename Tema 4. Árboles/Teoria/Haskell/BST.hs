------------------------------------------------------------
-- Estructuras de Datos
-- Tema 4. Árboles
-- Pablo López
--
-- Árboles de búsqueda en Haskell
------------------------------------------------------------

module BST where

import           Data.Maybe                        (isJust)
import           DataStructures.Graphics.DrawTrees
import           Test.QuickCheck

-- árboles binarios de búsqueda en Haskell
data BST a = Empty
           | Node a (BST a) (BST a) deriving Show
            --  root left   right

{-
   Aunque usamos deriving Show, no es muy útil; estas instancias son
   para pintar árboles binarios en formato gráfico. No es necesario
   saber crear estas instancias, se facilitan cuando sea necesario.
-}

instance Subtrees (BST a) where
    isEmptyTree Empty = True
    isEmptyTree _     = False

    subtrees Empty        = []
    subtrees (Node _ i d) = [i,d]

instance Show a => ShowNode (BST a) where
    showNode Empty        = ""
    showNode (Node r _ _) = show r

mkBST :: Ord a => [a] -> BST a
mkBST xs = foldr insert empty xs

tree1 :: BST Int
tree1 = mkBST [7, 4, 10, 17, 19, 18, 27, 45, 90, 55, 15, 20, 25, 40, 12,  68, 73, 75, 60, 70, 80,50]

inOrder :: BST a -> [a]
inOrder Empty        = []
inOrder (Node x i d) = inOrder i ++ x : inOrder d

treeSort :: Ord a => [a] -> [a]
treeSort xs = (inOrder . mkBST) xs

empty :: BST a
empty = Empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y lt rt) 
    | x == y = Node x lt rt 
    | x  < y = Node y (insert x lt) rt
    | x  > y = Node y lt (insert x rt)

search :: Ord a => a -> BST a -> Maybe a
search _ Empty = Nothing
search x (Node y lt rt)
    | x == y = Just x 
    | x  < y = search x lt
    | x  > y = search x rt

isElem :: Ord a => a -> BST a -> Bool
isElem x t = isJust (search x t)
