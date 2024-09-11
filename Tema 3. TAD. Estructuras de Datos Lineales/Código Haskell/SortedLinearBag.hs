-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 3 - Implementación y Especificación del TAD Bag
--
-- Alumno: SORIA GIL, ÁNGEL MANUEL
-------------------------------------------------------------------------------

module SortedLinearBag
  ( Bag
  , empty        -- :: Bag a
  , isEmpty      -- :: Bag a -> Bool
  , insert       -- :: Ord a => a -> Bag a -> Bag a
  , delete       -- :: Ord a => a -> Bag a -> Bag a
  , occurrences  -- :: Ord a => a -> Bag a -> Int
  , foldBag      -- :: Ord a => (a -> Int -> b -> b) -> b -> Bag a -> b
  , union        -- :: Ord a => Bag a -> Bag a -> Bag a
  , intersection -- :: Ord a => Bag a -> Bag a -> Bag a
  , difference   -- :: Ord a => Bag a -> Bag a -> Bag a
  ) where

import           Data.List       (sort, (\\))
import           Test.QuickCheck

{-
   Las funciones `sort` y `\\` del módulo `Data.List` se utilizan en las
   propiedades QuickCheck. No necesitas usarlas en la implementación.

      `sort xs`  - devuelve la ordenación de la lista `xs`
      `xs // ys` - devuelve la diferencia entre las listas `xs` e `ys`
-}

-- | Implementación del TAD Bag
-------------------------------------------------------------------------------

data Bag a = Empty
           | Node a Int (Bag a) -- elemento y contador de apariciones
           deriving Eq

{-
   Utilizaremos el siguiente invariante de representación:

      1) los nodos estarán ordenados por el elemento, sin repetidos

                      Nodo x ox (Nodo y oy s) implica que  x < y

      2) todos los contadores de apariciones deben ser positivos

                      Nodo x ox s  implica que  ox > 0

   Todas las operaciones del TAD Bag reciben una bolsa que satisface
   el invariante y, si devuelven una bolsa, ésta debe satisfacer el
   invariante.
-}

-- invariante: ordenado, sin repetidos y con contadores positivos
bolsa1 :: Bag Char
bolsa1 = Node 'a' 2 (Node 'b' 3 (Node 'c' 2 (Node 'd'  1 Empty)))

mkBag :: Ord a => [a] -> Bag a
mkBag xs = foldr insert empty xs

{-
   La función `mkBag` permite convertir una lista en una bolsa.
   Por ejemplo:

      mkBag "abracadabra"

   devuelve la bolsa:

      Node 'a' 5 (Node 'b' 2 (Node 'c' 1 (Node 'd' 1 (Node 'r' 2 Empty))))

   que se representa mediante `show` como:

      LinearBag { 'a' 'a' 'a' 'a' 'a' 'b' 'b' 'c' 'd' 'r' 'r' }

   Puedes usar `mkBag` para construir bolsas para comprobar las
   funciones de bolsas. Por ejemplo para probar `delete` podemos usar:

      > delete 'a' (mkBag "haskell")
      LinearBag { 'e' 'h' 'k' 'l' 'l' 's' }

      > delete 'l' (mkBag "haskell")
      LinearBag { 'a' 'e' 'h' 'k' 'l' 's' }

      > delete 'x' (mkBag "haskell")
      LinearBag { 'a' 'e' 'h' 'k' 'l' 'l' 's' }

      > delete 'a' (mkBag "haskell") == mkBag "hskell"
      True
-}

-- devuelve una bolsa vacía
-- |
-- >>> empty
-- LinearBag { }
empty :: Bag a
empty = Empty

-- comprueba si una bolsa está vacía
-- |
-- >>> isEmpty empty
-- True
-- >>> isEmpty bolsa1
-- False
isEmpty :: Bag a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- inserta un nuevo dato en una bolsa
-- |
-- >>> insert 'z' bolsa1
-- LinearBag { 'a' 'a' 'b' 'b' 'b' 'c' 'c' 'd' 'z' }
-- >>> insert 'b' bolsa1
-- LinearBag { 'a' 'a' 'b' 'b' 'b' 'b' 'c' 'c' 'd' }
insert :: Ord a => a -> Bag a -> Bag a
insert elem1 Empty = Node elem1 1 Empty
insert elem1 (Node elem2 c bs)
   | elem1 == elem2 = Node elem2 (c+1) bs
   | elem1 > elem2  = Node elem2 c (insert elem1 bs)
   | elem1 < elem2  = Node elem1 1 (Node elem2 c bs)


-- devuelve el número de apariciones de un elemento en una bolsa
-- (0 si el elemento no está en la bolsa)
-- |
-- >>> occurrences 'a' bolsa1
-- 2
-- >>> occurrences 'w' bolsa1
-- 0
occurrences :: (Ord a) => a -> Bag a -> Int
occurrences _ Empty = 0
occurrences elem1 (Node elem2 c bs) 
   | elem1 == elem2 = c
   | otherwise      = occurrences elem1 bs 

-- borra una ocurrencia de un dato de una bolsa
-- (devuelve la bolsa original si el dato no estaba en la bolsa)
-- |
-- >>> delete 'a' bolsa1
-- LinearBag { 'a' 'b' 'b' 'b' 'c' 'c' 'd' }
-- >>> delete 'd' bolsa1
-- LinearBag { 'a' 'a' 'b' 'b' 'b' 'c' 'c' }
-- >>> delete 'w' bolsa1
-- LinearBag { 'a' 'a' 'b' 'b' 'b' 'c' 'c' 'd' }
delete :: (Ord a) => a -> Bag a -> Bag a
delete _ Empty = Empty
delete elem1 (Node elem2 c bs)
   | elem1 == elem2 && (c-1)==0       = bs
   | elem1 == elem2 && (not ((c-1)==0)) = Node elem2 (c-1) bs
   | otherwise                        = Node elem2 c (delete elem1 bs)

-- instancia de la clase `Show` para imprimir las bolsas
instance (Show a) => Show (Bag a) where
   show s = "LinearBag { " ++ show' s
    where
       show' Empty         = "}"
       show' (Node x ox s) = muestra x ox ++  show' s
       muestra x 0  = ""
       muestra x ox = show x ++ ' ' : muestra x (ox-1)

-- | Especificación del TAD Bag
-------------------------------------------------------------------------------

-- generación de bolsas aleatorias para QuickCheck
instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
  arbitrary = do
                 xs <- listOf arbitrary
                 return (foldr insert empty xs)

-- selectores
isEmpty_empty :: Bool
isEmpty_empty = isEmpty Empty == True

isEmpty_insert :: Ord a => a -> Bag a -> Bool
isEmpty_insert elem bs = isEmpty (insert elem bs) == False 

occurrences_empty :: Ord a => a -> Bool
occurrences_empty x = (occurrences x Empty) == 0

occurrences_insert_1 :: Ord a => a -> Bag a -> Bool
occurrences_insert_1 x bs = (occurrences x (insert x bs)) >= 1 

occurrences_insert_2 :: Ord a => a -> a -> Bag a -> Property
occurrences_insert_2 x y bs = x == y ==> occurrences x (insert y (insert x bs)) >= 2

-- transformadores
delete_empty :: Ord a => a -> Bool
delete_empty x = isEmpty (delete x Empty) 

delete_insert_1 :: Ord a => a -> Bag a -> Bool
delete_insert_1 x bs = delete x (insert x bs) == bs

delete_insert_2 :: Ord a => a -> a -> Bag a -> Property
delete_insert_2 x y bs = x==y ==> delete x (insert y bs) == bs

type T = Char -- Integer, etc.

check_Bag = do
               quickCheck (isEmpty_empty :: Bool)
               quickCheck (isEmpty_insert :: T -> Bag T -> Bool)
               quickCheck (occurrences_empty :: T -> Bool)
               quickCheck (occurrences_insert_1 :: T -> Bag T -> Bool)
               quickCheck (occurrences_insert_2 :: T -> T -> Bag T -> Property)
               quickCheck (delete_empty :: T -> Bool)
               quickCheck (delete_insert_1 :: T -> Bag T -> Bool)
               quickCheck (delete_insert_2 :: T -> T -> Bag T -> Property)

-- | Operaciones auxiliares del TAD Bag
-------------------------------------------------------------------------------

{-
   Añadir al módulo las siguientes funciones auxliares para bolsas:

      - unión de bolsas
      - intersección de bolsas
      - diferencia de bolsas

   Estas funciones son semejantes a las de los conjuntos pero teniendo
   en cuenta las apariciones de cada elemento.
-}

b1 :: Bag Char
b1 = mkBag "haskell"

b2 :: Bag Char
b2 = mkBag "java"

-- |
-- >>> union b1 b2
-- LinearBag { 'a' 'a' 'a' 'e' 'h' 'j' 'k' 'l' 'l' 's' 'v' }
union :: Ord a => Bag a -> Bag a -> Bag a
union Empty Empty = Empty
union Empty (Node y cy by) = (Node y cy by)
union (Node x cx bx) Empty = (Node x cx bx)
union (Node x cx bx) (Node y cy by) 
   | x == y = (Node x (cx+cy) (union bx by))
   | x < y  = Node x cx (union bx (Node y cy by))
   | y < x  = Node y cy (union by (Node x cx bx))


-- |
-- >>> intersection b1 b2
-- LinearBag { 'a' }
intersection :: Ord a => Bag a -> Bag a -> Bag a
intersection Empty Empty = Empty
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection (Node x cx bx) (Node y cy by)
   | x == y = Node x (min cx cy) (intersection bx by)
   | x < y  = intersection bx (Node y cy by)
   | x > y  = intersection (Node x cx bx) by 

-- |
-- >>> difference b1 b2
-- LinearBag { 'e' 'h' 'k' 'l' 'l' 's' }
-- >>> difference b2 b1
-- LinearBag { 'a' 'j' 'v' }
difference :: Ord a => Bag a -> Bag a -> Bag a
difference Empty Empty = Empty
difference Empty _     = Empty
difference bs Empty    = bs 
difference (Node x cx bx) (Node y cy by)
   | x == y && (cx-cy > 0)  = Node x (cx-cy) (difference bx by)
   | x == y && (cx-cy <= 0) = difference bx by
   | x < y                  = Node x cx (difference bx (Node y cy by))
   | x > y                  = difference (Node x cx bx) by

{-
   Utiliza estas propiedades QuickCheck para comprobar la
   implementación de `union`, `intersection` y `difference`
-}

check_union xs ys =
   union (mkBag xs) (mkBag ys) == mkBag (xs ++ ys)

check_intersection xs ys =
   intersection (mkBag xs) (mkBag ys) == mkBag (intersecta (sort xs) (sort ys))
    where
      intersecta [] _ = []
      intersecta _ [] = []
      intersecta (x:xs) (y:ys)
        | x == y = x : intersecta xs ys
        | x <  y = intersecta xs (y:ys)
        | otherwise = intersecta (x:xs) ys

check_difference xs ys =
   difference (mkBag xs) (mkBag ys) == mkBag (xs \\ ys)

check_Bag_Aux = do
                   quickCheck (check_union :: [T] -> [T] -> Bool)
                   quickCheck (check_intersection :: [T] -> [T] -> Bool)
                   quickCheck (check_difference :: [T] -> [T] -> Bool)

-------------------------------------------------------------------------------
-- Eficiencia de la implementación del TAD Bag
-------------------------------------------------------------------------------

{-
   Responde a las dos siguientes preguntas sobre la eficiencia de nuestra
   representación del TAD Bag.

   1) Completa la siguiente tabla indicando mediante la notación O el número
   de pasos que realiza cada operación del TAD Bag

       operación        números de pasos
       ---------------------------------
       empty                O(1)
       isEmpty              O(1)
       insert               O(n)
       delete               O(n)
       occurrences          O(n)

   2) Nuestra representación del TAD Bag mantiene la secuencia de
   nodos ordenados:

           Node 'a' 2 (Node 'b' 3 (Node 'c' 2 (Node 'd'  1 Empty)))

   Obviamente, también es posible representar el TAD Bag por una
   secuencia desordenada:

           Node 'c' 2 (Node 'd' 1 (Node 'a' 2 (Node 'b'  3 Empty)))

   ¿Cuál de las dos representaciones es más eficiente? ¿Por qué?
   La ordenada es más eficiente pues para ciertas operaciones nos permitirá buscar o 
   recorrer solamente parte del SortedLinearBag y no todo entero, no obstante, los peores 
   casos no se pueden evitar

-}

-- | Plegado del TAD Bag
-------------------------------------------------------------------------------

-- función de plegado para bolsas
foldBag :: Ord a => (a -> Int -> b -> b) -> b -> Bag a -> b
foldBag f solBase xs = plegar xs
   where
      plegar Empty         = solBase
      plegar (Node x ox s) = f x ox (plegar s)
