-------------------------------------------------------------------------------
-- Apellidos, Nombre: Alcausa Luque, Juan Carlos
-- Titulacion, Grupo: 
--
-- Estructuras de Datos. Grados en Informatica. UMA.
-------------------------------------------------------------------------------

module AVLBiDictionary( BiDictionary
                      , empty
                      , isEmpty
                      , size
                      , insert
                      , valueOf
                      , keyOf
                      , deleteByKey
                      , deleteByValue
                      , toBiDictionary
                      , compose
                      , isPermutation
                      , orbitOf
                      , cyclesOf
                      ) where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.Set.BSTSet               as S

import           Data.List                               (intercalate, nub,
                                                          (\\))
import           Data.Maybe                              (fromJust, fromMaybe,
                                                          isJust)
import           Test.QuickCheck


data BiDictionary a b = Bi (D.Dictionary a b) (D.Dictionary b a)

-- | Exercise a. empty, isEmpty, size

empty :: (Ord a, Ord b) => BiDictionary a b
empty = Bi (D.empty) (D.empty)

isEmpty :: (Ord a, Ord b) => BiDictionary a b -> Bool
isEmpty (Bi d1 d2) = D.isEmpty d1 && D.isEmpty d2

size :: (Ord a, Ord b) => BiDictionary a b -> Int
size (Bi d1 d2) = D.size d1

-- | Exercise b. insert

insert :: (Ord a, Ord b) => a -> b -> BiDictionary a b -> BiDictionary a b
insert x y (Bi d1 d2)
    | (D.isDefinedAt x d1) = (Bi (D.insert x y d1) (D.insert y x (D.delete (fromJust(D.valueOf x d1)) d2)))
    | otherwise = (Bi (D.insert x y d1) (D.insert y x d2))

-- | Exercise c. valueOf

valueOf :: (Ord a, Ord b) => a -> BiDictionary a b -> Maybe b
valueOf x (Bi d1 d2)
    | (D.isDefinedAt x d1) = D.valueOf x d1
    | otherwise = Nothing

-- | Exercise d. keyOf

keyOf :: (Ord a, Ord b) => b -> BiDictionary a b -> Maybe a
keyOf y (Bi d1 d2)
    | D.isDefinedAt y d2 = D.valueOf y d2
    | otherwise = Nothing

-- | Exercise e. deleteByKey

deleteByKey :: (Ord a, Ord b) => a -> BiDictionary a b -> BiDictionary a b
deleteByKey x bi@(Bi d1 d2)
    | not (D.isDefinedAt x d1) = bi
    | otherwise = Bi (D.delete x d1) (D.delete (fromJust (valueOf x bi)) d2)

-- | Exercise f. deleteByValue

deleteByValue :: (Ord a, Ord b) => b -> BiDictionary a b -> BiDictionary a b
deleteByValue y bi@(Bi d1 d2)
    | not (D.isDefinedAt y d2) = bi
    | otherwise = Bi (D.delete (fromJust (keyOf y bi)) d1) (D.delete y d2)

-- | Exercise g. toBiDictionary

toBiDictionary :: (Ord a, Ord b) => D.Dictionary a b -> BiDictionary a b
toBiDictionary dict = aux (D.keysValues dict) empty
    where
        aux [] (Bi d1 d2) = Bi d1 d2
        aux ((x,y):xs) bi@(Bi d1 d2)
            | (D.isDefinedAt x d1) || (D.isDefinedAt y d2) = error "Diccionario no inyectivo"
            | otherwise = aux xs (Bi (D.insert x y d1) (D.insert y x d2))

-- | Exercise h. compose

compose :: (Ord a, Ord b, Ord c) => BiDictionary a b -> BiDictionary b c -> BiDictionary a c
compose bi1@(Bi d11 d12) bi2@(Bi d21 d22) = aux list empty
    where
        list = [(x,y) | x <- D.keys d11, y <- D.values d21, y == fromJust(D.valueOf (fromJust (D.valueOf x d11)) d21)]
        aux [] res = res
        aux ((x,y):xs) res = aux xs (insert x y res)

-- | Exercise i. isPermutation

isPermutation :: Ord a => BiDictionary a a -> Bool
isPermutation (Bi d1 d2) = D.keys d1 == D.values d1



-- |------------------------------------------------------------------------


-- | Exercise j. orbitOf

orbitOf :: Ord a => a -> BiDictionary a a -> [a]
orbitOf = undefined

-- | Exercise k. cyclesOf

cyclesOf :: Ord a => BiDictionary a a -> [[a]]
cyclesOf = undefined

-- |------------------------------------------------------------------------


instance (Show a, Show b) => Show (BiDictionary a b) where
  show (Bi dk dv)  = "BiDictionary(" ++ intercalate "," (aux (D.keysValues dk)) ++ ")"
                        ++ "(" ++ intercalate "," (aux (D.keysValues dv)) ++ ")"
   where
    aux kvs  = map (\(k,v) -> show k ++ "->" ++ show v) kvs
