-------------------------------------------------------------------------------
-- Apellidos, Nombre: Ángel Manuel Soria Gil
-- Titulacion, Grupo: Doble Grado Matemáticas e Ingeniería Informática
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
isEmpty (Bi dk dv) = D.isEmpty dk && D.isEmpty dv

size :: (Ord a, Ord b) => BiDictionary a b -> Int
size (Bi dk dv) = D.size dk
 
-- | Exercise b. insert

insert :: (Ord a, Ord b) => a -> b -> BiDictionary a b -> BiDictionary a b
insert k v (Bi dk dv)
  | k `elem` D.values dv = (Bi (D.insert k v dk) (D.insert v k (D.delete (fromJust (D.valueOf k dk)) dv)))
  | v `elem` D.values dk = (Bi (D.insert k v (D.delete (fromJust (D.valueOf v dv)) dk)) (D.insert v k dv))
  | otherwise            = (Bi (D.insert k v dk) (D.insert v k dv))

-- | Exercise c. valueOf

valueOf :: (Ord a, Ord b) => a -> BiDictionary a b -> Maybe b
valueOf k (Bi dk dv) = D.valueOf k dk

-- | Exercise d. keyOf

keyOf :: (Ord a, Ord b) => b -> BiDictionary a b -> Maybe a
keyOf v (Bi dk dv) = D.valueOf v dv

-- | Exercise e. deleteByKey

deleteByKey :: (Ord a, Ord b) => a -> BiDictionary a b -> BiDictionary a b
deleteByKey k (Bi dk dv) 
  | not (D.isDefinedAt k dk) = Bi dk dv
  | otherwise                = Bi (D.delete k dk) (D.delete v dv)
  where 
    v = fromJust (D.valueOf k dk)   
      

-- | Exercise f. deleteByValue

deleteByValue :: (Ord a, Ord b) => b -> BiDictionary a b -> BiDictionary a b
deleteByValue v (Bi dk dv)
  | not (D.isDefinedAt v dv) = Bi dk dv
  | otherwise                = Bi (D.delete k dk) (D.delete v dv)
  where
    k = fromJust (D.valueOf v dv)

-- | Exercise g. toBiDictionary

toBiDictionary :: (Ord a, Ord b) => D.Dictionary a b -> BiDictionary a b
toBiDictionary dict  
  | not (inyective dict) = error "error: toBiDictionary applied on non inyective dictionary"
  | otherwise            = Bi dict (reverseDict dict)

reverseDict :: (Ord a, Ord b) => D.Dictionary a b -> D.Dictionary b a
reverseDict dict = insertAux (D.keysValues dict) D.empty
  where 
    insertAux ((k,v):rest) dic = insertAux rest (D.insert v k dic)
    insertAux [] dic           = dic 

inyective :: (Ord a, Ord b) => D.Dictionary a b -> Bool
inyective dict = foldr f True (D.values dict)
  where
    f elem solAc = appearences elem (D.values dict) == 1 && solAc

appearences :: (Ord a) => a -> [a] -> Integer
appearences x list = foldr f 0 list
  where 
    f elem solAc
      | elem == x = solAc + 1
      | otherwise = solAc

-- | Exercise h. compose

compose :: (Ord a, Ord b, Ord c) => BiDictionary a b -> BiDictionary b c -> BiDictionary a c
compose (Bi da1 db1) (Bi db2 dc2) = insertAux list empty
  where
    list = [(k,v) | k <- D.keys da1, v <- D.values db2, v == fromJust (D.valueOf (fromJust (D.valueOf k da1)) db2)]
    insertAux ((k,v):rest) bidic = insertAux rest (insert k v bidic)
    insertAux [] bidic           = bidic

-- | Exercise i. isPermutation

isPermutation :: Ord a => BiDictionary a a -> Bool
isPermutation (Bi dk dv) = foldr f True list
  where
    list = D.keys dk
    f x solAc 
      | x `elem` codomain = solAc
      | otherwise            = False && solAc
      where
        codomain = D.values dk




-- |------------------------------------------------------------------------


-- | Exercise j. orbitOf

orbitOf :: Ord a => a -> BiDictionary a a -> [a]
orbitOf x (Bi dk dv)
  | not (isPermutation (Bi dk dv)) = error "you shoudn't have done this"
  | otherwise = orbitOfAux x dk []
  where 
    orbitOfAux x dict solAc
      | y `elem` solAc = solAc++[x]
      | otherwise      = orbitOfAux y dict (solAc++[x])
      where 
        y = fromJust (D.valueOf x dict)

-- | Exercise k. cyclesOf

cyclesOf :: Ord a => BiDictionary a a -> [[a]]
cyclesOf (Bi dk dv) = ciclesOfAux (D.keys dk) (Bi dk dv)
  where 
    ciclesOfAux list (Bi dk dv) = foldr f [] list
      where 
        f x solAc
          | x `elemelem` solAc = solAc
          | otherwise          = solAc ++ [(orbitOf x (Bi dk dv))]  

elemelem :: Ord a => a -> [[a]] -> Bool
elemelem x list = elem x list'
  where
    list' = [x | v <- list, x <- v]

-- |------------------------------------------------------------------------


instance (Show a, Show b) => Show (BiDictionary a b) where
  show (Bi dk dv)  = "BiDictionary(" ++ intercalate "," (aux (D.keysValues dk)) ++ ")"
                        ++ "(" ++ intercalate "," (aux (D.keysValues dv)) ++ ")"
   where
    aux kvs  = map (\(k,v) -> show k ++ "->" ++ show v) kvs
