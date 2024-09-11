-------------------------------------------------------------------------------
-- Estructuras de Datos
-- Práctica 9 - Orden topológico de un digrafo (sin clonar el grafo)
-- Ángel Manuel Soria Gil
-- 2º Doble Grado Matemáticas + Ingeniería Informática
-- No están bien
-------------------------------------------------------------------------------

module TopologicalSortingDic
  ( topSorting
  , topSortings
  ) where

import           DataStructures.Dictionary.AVLDictionary as D
import           DataStructures.Graph.DiGraph

-- Decrement 1 the value associated to key v
decrementValue :: (Num b, Ord a) => a -> Dictionary a b -> Dictionary a b
decrementValue v dic = D.insert v (u-1) dic
    where
    Just u = D.valueOf v dic

-- Remove all keys from the dic
removeKeys :: (Ord a) => [a] -> Dictionary a b -> Dictionary a b
removeKeys keys dic = foldr D.delete dic keys

-- compute topological order
topSorting :: (Ord a) => DiGraph a -> [a]
topSorting g  = aux initPenPred
 where
  initPenPred = foldr D.insert D.empty kvs
  aux pendingPred
    | D.isEmpty pendingPred = []
    | null srcs             = error "DiGraph is cyclic"
    | otherwise             = [k | k <- srcs] : aux pendingPred'
    where
      srcs = [k | k <- D.keys pendingPred, D.valueOf k pendingPred == 0]
      kvs = [(k, v) | k <- (vertices g), v == length(predecesors g k)]
      pendingPred' = foldr decrementValue (removeKeys srcs pendingPred) [k | k <- successors g v, v <- srcs]

-- compute topological order in nested collection
topSortings :: (Ord a) => DiGraph a -> [[a]]
topSortings g  = aux initPenPred
  where
    initPenPred = foldr D.insert D.empty kvs
    aux pendingPred
      | D.isEmpty pendingPred = []
      | null srcs             = error "DiGraph is cyclic"
      | otherwise             = [[k | k <- srcs]] : aux pendingPred'
      where
        srcs = [k | k <- D.keys pendingPred, D.valueOf k pendingPred == 0]
        kvs  = [(k, v) | k <- (vertices g), v == length(predecesors g k)]
        pendingPred' = foldr decrementValue (removeKeys srcs pendingPred) [k | k <- successors g v, v <- srcs]
