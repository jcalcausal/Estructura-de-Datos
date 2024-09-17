-------------------------------------------------------------------------------
-- Estructuras de Datos
-- Práctica 9 - Orden topológico de un digrafo (sin clonar el grafo)
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
  initPenPred = foldr insertDicAux D.empty aList
    where 
      aList                         = [(k, v) | k <- (vertices g), let v = length (predecesors g k)]
      insertDicAux (key, value) dic = D.insert key value dic
  aux pendingPred
    | D.isEmpty pendingPred = []
    | null srcs             = error "DiGraph is cyclic"
    | otherwise             = srcs ++ (aux newDic)
    where
      srcs   = [k | k <- (keys pendingPred), (valueOf k pendingPred) == Just 0] -- list of vertices with 0 pending predecessors
      newDic = removeKeys srcs decrementedPendingPreds
        where 
          decrementedPendingPreds = foldr decrementValue pendingPred aList2
            where
              aList2 = [v | x <- srcs, v <- (successors g x)]


-- compute topological order in nested collection
topSortings :: (Ord a) => DiGraph a -> [[a]]
topSortings g  = aux initPenPred
  where
    initPenPred = foldr insertDicAux D.empty aList
      where
        aList                         = [(k, v) | k <- (vertices g), let v = length (predecesors g k)]
        insertDicAux (key, value) dic = D.insert key value dic
    aux pendingPred
      | D.isEmpty pendingPred = []
      | null srcs             = error "DiGraph is cyclic"
      | otherwise             = [srcs] ++ (aux newDic)
      where
      srcs   = [k | k <- (keys pendingPred), (valueOf k pendingPred) == Just 0] -- list of vertices with 0 pending predecessors
      newDic = removeKeys srcs decrementedPendingPreds
        where 
          decrementedPendingPreds = foldr decrementValue pendingPred aList2
            where
              aList2 = [v | x <- srcs, v <- (successors g x)]