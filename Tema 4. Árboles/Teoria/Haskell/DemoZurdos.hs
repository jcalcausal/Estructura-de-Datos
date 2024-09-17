------------------------------------------------------------
-- Estructuras de Datos
-- Tema 4. Árboles
-- Pablo López
--
-- Programa para probar los montículos zurdos
------------------------------------------------------------

module DemoZurdos where

import           Data.List                         (nub)
import           DataStructures.Graphics.DrawTrees
import           DataStructures.Heap.WBLeftistHeap

outputFile :: String
outputFile = "zurdo.svg"

drawHeap :: Show a => Heap a -> IO ()
drawHeap h = drawOn outputFile h

drawCharHeap :: Heap Char -> IO ()
drawCharHeap h = drawOnWith outputFile (:[]) h

-- crea y dibuja un (Heap a) en outputFile
demo1 :: (Show a, Ord a) => [a] -> IO ()
demo1 xs = drawHeap (mkHeap xs)

-- crea y dibuja un (Heap Char) en outputFile
demo2 :: String -> IO ()
demo2 xs = drawCharHeap (mkHeap xs)

-- construye un heap de manera ineficiente O(n^2)
buildHeap :: Ord a => [a] -> Heap a
buildHeap xs = foldr insert empty xs

-- ordenación eficiente O(n log n) basada en mkHeap
heapSort :: (Ord a) => [a] -> [a]
heapSort = heapToList . mkHeap

heapToList :: Ord a => Heap a -> [a]
heapToList h
 | isEmpty h  = []
 | otherwise = minElem h : heapToList (delMin h)
