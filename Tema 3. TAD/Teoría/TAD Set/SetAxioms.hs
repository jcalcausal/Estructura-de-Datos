------------------------------------------------------------
-- Estructuras de Datos
-- Tema 3. Estructuras de Datos Lineales
-- Pablo López
--
-- Módulo de la especificación del TAD Conjunto
------------------------------------------------------------

module SetAxioms where

-- import           DataStructures.Set.SortedLinearSet
import           Set
import           Test.QuickCheck

{-
   Para el TAD Conjunto, clasificamos sus operaciones en:

      - constructores: `empty`, `insert`
      - selectores: `isEmpty`, `isElem`
      - transformadores: `delete`

   Con los constructores podemos construir cualquier valor del TAD
   Conjunto de la siguiente manera:

         insert 5 (insert 1 (insert 7 (insert 1 (insert 3 empty))))

   Observa que es posible insertar elementos en cualquier orden y es
   posible insertar elementos que ya pertenecen al conjunto.

   Para especificar formalmente el TAD Conjunto debemos especificar
   qué devuelven los selectores y transformadores cuando se aplican a
   los constructores.
-}

-- ¿Qué devuelve `isEmpty` cuando se aplica a un constructor?

ax_isEmpty_empty :: Bool
ax_isEmpty_empty = isEmpty empty == True

ax_isEmpty_insert :: Ord a => a -> Set a -> Bool
ax_isEmpty_insert x s = isEmpty (insert x s) == False

-- ¿Qué devuelve `isElem` cuando se aplica a un constructor?

ax_isElem_empty :: Ord a => a -> Bool
ax_isElem_empty x = isElem x empty == False

ax_isElem_insert :: Ord a => a -> a -> Set a -> Bool
ax_isElem_insert x y s = isElem x (insert y s) == (x == y || isElem x s)

--   ¿Qué devuelve delete cuando se aplica a un constructor?

ax_delete_empty :: Ord a => a -> Bool
ax_delete_empty x = delete x empty == empty

ax_delete_insert_1 :: Ord a => a -> a -> Set a -> Property
ax_delete_insert_1 x y s = x == y ==> delete x (insert y s) == delete x s

ax_delete_insert_2 :: Ord a => a -> a -> Set a -> Property
ax_delete_insert_2 x y s = x /= y ==> delete x (insert y s) == insert y (delete x s)

{-
   La siguiente instancia de `Arbitrary` es para enseñar a QuickCheck a generar
   Set aleatorios: no hay que saber cómo hacerlo; siempre se facilita.

   Definimos un sinónimo de tipo `T` para realizar las pruebas de
   QuickCheck sobre un tipo monomórfico.
-}

type T = Integer -- o Char, etc.

check_isEmpty_empty  = quickCheck (ax_isEmpty_empty :: Bool)
check_isEmpty_insert = quickCheck (ax_isEmpty_insert :: T -> Set T -> Bool)
check_isElem_empty   = quickCheck (ax_isElem_empty :: T -> Bool)
check_isElem_insert  = quickCheck (ax_isElem_insert :: T -> T -> Set T -> Bool)
check_delete_empty   = quickCheck (ax_delete_empty :: T -> Bool)
check_delete_insert_1  = quickCheck (ax_delete_insert_1 :: T -> T -> Set T -> Property)
check_delete_insert_2  = quickCheck (ax_delete_insert_2 :: T -> T -> Set T -> Property)

-- La función `check_Set` permite comprobar todas las propiedades QuickCheck.

check_Set = do
               check_isEmpty_empty
               check_isEmpty_insert
               check_isElem_empty
               check_isElem_insert
               check_delete_empty
               check_delete_insert_1
               check_delete_insert_2
