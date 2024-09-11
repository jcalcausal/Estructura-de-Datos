--------------------------------------------------------------------------------------------
-- Ángel Manuel Soria Gil 
-- TAD SetMultiMap
--------------------------------------------------------------------------------------------

module SetMultiMap 
    (SetMultiMap
    , empty
 --   , isEmpty
 --   , size
 --   , isDefinedAt
 --   , insert
 --   , deleteKey
 --   , deleteKeyValue
 --   , valuesOf
 --   , filterValues
  --  , fold
    ) where 

import Data.List (intercalate)
import Test.QuickCheck
import qualified DataStructures.Set.LinearSet as S

-- Invariente de Representación:
-- - Nodos ordenados por claves
-- - No hay claves repetidas
-- - No hay claves que tengan asociado un conjunto vacio

data SetMultiMap a b = Empty 
                     | Node a (S.Set b) (SetMultiMap a b)
                     deriving Eq

-- Ejemplo de SetMultiMap para probar las funciones
m1 :: SetMultiMap String Int 
m1 = Node "Alfredo" (mkSet [9]) (
     Node "Juan"    (mkSet [0, 1, 8]) (
     Node "Maria"   (mkSet [4, -6, 8])
     Empty)
    )

mkSet :: Eq a => [a] -> S.Set a 
mkSet = foldr S.insert S.empty

-- empty : crea un SetMultiMap vacío
-- Complejidad O(1)
empty :: SetMultiMap a b
empty = Empty

-- isEmpty : devuelve si un setMultiMap es vacío
-- Complejidad O(1)
isEmpty :: SetMultiMap a b -> Bool
isEmpty Empty = True
isEmpty _ = False 

-- size : devuelve el tamaño del setMultiMap
-- Complejidad O(n)  pues recorre todo el SMM
size :: SetMultiMap a b -> Integer
size Empty = 0 
size (Node a vals restoSMM) = 1 + size restoSMM 

-- isDefinedAt : devuelve si una key está en nuestro SMM
-- Complejidad O(n) recorre en el peor de los casos todo el SMM
isDefinedAt :: (Ord a, Eq a) => a -> SetMultiMap a b -> Bool
isDefinedAt _ Empty                    = False
isDefinedAt key (Node a vals restoSMM) = key == a || isDefinedAt key restoSMM

-- insert : añade un key y unos valores al smm
-- Complejidad O(n) recorre en el peor de los casos todo el SMM
insert :: (Ord a, Eq b) => a -> b ->  SetMultiMap a b -> SetMultiMap a b 
insert k1 v1 Empty = Node k1 (S.insert v1 S.empty) Empty
insert k1 v1 (Node k2 v2 restoSMM) 
    | k1 == k2 = (Node k2 (S.insert v1 v2) restoSMM)
    | k1 <  k2 = (Node k1 (S.insert v1 S.empty) (Node k2 v2 restoSMM))
    | k1 >  k2 = (Node k2 v2 (insert k1 v1 restoSMM)) 

-- deleteKey : Borra una clave del SMM
-- Complejidad O(n) pues recorre en el peor caso todo el SMM
deleteKey :: (Ord a, Eq b) => a -> SetMultiMap a b -> SetMultiMap a b 
deleteKey k1 Empty = error "Aplying deleteKey on empty SetMultiMap"
deleteKey k1 (Node k2 vals2 restoSMM) 
    | k1 == k2 = restoSMM
    | k1 <  k2 = error "Not found key on this SetMultiMap"
    | k1 >  k2 = (Node k2 vals2 (deleteKey k1 restoSMM))

-- deleteKeyValue : Borra un valor para una clave
-- Complejidad O(n) pues recorre en el peor caso todo el SMM
deleteKeyValue :: (Ord a, Eq b) => a -> b -> SetMultiMap a b -> SetMultiMap a b 
deleteKeyValue _ _ Empty = error "Aplying deleteKeyValue on empty SetMultiMap"
deleteKeyValue k1 v1 (Node k2 vals2 restoSMM)
    | k1 == k2 = if (S.isEmpty (S.delete v1 vals2)) 
                    then deleteKey k1 (Node k2 vals2 restoSMM)
                     else (Node k2 (S.delete v1 vals2) restoSMM)
    | k1 <  k2 = error "Not found key on this SetMultimap"
    | k1 >  k2 = Node k2 vals2 (deleteKeyValue k1 v1 restoSMM) 

-- valuesOf : devuelve los valores de una clave
-- Complejidad O(n) 
valuesOf :: (Ord a, Eq b) => a -> SetMultiMap a b -> Maybe (S.Set b)
valuesOf _ Empty = Nothing
valuesOf k1 (Node k2 vals2 restoSMM) 
    | k1 == k2 = Just vals2
    | k1 <  k2 = Nothing
    | k1 >  k2 = valuesOf k1 restoSMM

-- filterValues : filtra los valores según una propiedad 
filterValues :: (Ord a, Eq b) => (b -> Bool) -> SetMultiMap a b -> SetMultiMap a b 
filterValues p Empty = Empty
filterValues p (Node k1 vals1 restoSMM) = Node k1 vals 
    





fold :: (Ord a, Eq b) => (a -> b -> c -> c) -> c -> SetMultiMap a b -> c 
fold f z ms = recSetMultiMap ms
 where
    recSetMultiMap Empty = z 
    recSetMultiMap (Node k s ms) 
        | S.isEmpty s = recSetMultiMap ms 
        | otherwise = f k v (recSetMultiMap (Node k s' ms)) 
        where (v, s') = pickOne s
    pickOne s = (v, S.delete v s)
        where v = head $ S.fold (:) [] s
    

instance (Show a, Show b) => Show(SetMultiMap a b) where 
    show Empty              = "{}"
    show ms                 = intercalate "\n" (showKeyValues ms)
        where
            showKeyValues Empty = []
            showKeyValues (Node k s ms) = (show k ++ " --> " ++ show s) : showKeyValues ms


instance (Ord a, Arbitrary a, Eq b, Arbitrary b) => Arbitrary (SetMultiMap a b)
 where     
    arbitrary = do 
        xs <- listOf arbitrary 
        ys <- listOf arbitrary
        return (foldr (uncurry insert) empty (zip xs ys))