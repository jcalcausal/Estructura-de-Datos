------------------------------------------------------------
-- Estructuras de Datos
-- Tema 3. Estructuras de Datos Lineales
-- Pablo López
--
-- Módulo de la especificación del TAD Cola
------------------------------------------------------------

module QueueAxioms where

import           Queue
-- import           DataStructures.Queue.LinearQueue

import           Test.QuickCheck

-- | Especificación del TAD Queue
------------------------------------------------------------
{-
   Las operaciones de un TAD se clasifican en:

        - constructores
        - selectores
        - transformadores

   Para especificar formalmente un TAD hay que especificar qué devuelve:

        - cada selector y
        - cada transformador

   cuando se aplica a cada constructor.

   Para el TAD Cola tenemos dos constructores: `enqueue` y `empty`.
   El resto de operaciones son:

        - selectores (`isEmpty`, `first`)
        - transformadores (`dequeue`).

   Por ejemplo, para especificar la operación `isEmpty` tenemos
   que especificar qué devuelve `isEmpty` cuando se aplica a una Cola
   construida con `empty` o con `enqueue`, etc.

   Recuerda que al especificar los axiomas trabajamos con colas construidas
   exclusivamente con los constructores `enqueue` y `empty`; por ejemplo:

            enqueue 7 (enqueue 1 (enqueue 5 (enqueue 3 empty)))

   Ejercicio: En la cola anterior, ¿qué dato es el primero y qué dato es
   el último?

   Los axiomas que definen el TAD Cola son los siguientes:

         isEmpty empty = true
         isEmpty (enqueue x q) = false

         first (enqueue x q) = first q

         dequeue (enqueue x q) = ???

   ¿Qué implican los axiomas sobre `first` y `dequeue`?

   Cada axioma de la especificación formal se representa mediante
   un propiedad QuickCheck.
-}

-- ¿Qué devuelve `isEmpty` cuando se aplica a un constructor?

ax_isEmpty_empty :: Bool
ax_isEmpty_empty = isEmpty empty == True

ax_isEmpty_enqueue :: a -> Queue a -> Bool
ax_isEmpty_enqueue x q = isEmpty (enqueue x q) == False

-- ¿Qué devuelve `first` cuando se aplica a un constructor?

ax_first_enqueue_1 :: Eq a => a -> Bool
ax_first_enqueue_1 x = first (enqueue x empty) == x

ax_first_enqueue_2 :: Eq a => a -> Queue a -> Property
ax_first_enqueue_2 x q = not (isEmpty q) ==> first (enqueue x q) == first q

-- ¿Qué devuelve `dequeue` cuando se aplica a un constructor?

ax_dequeue_enqueue_1 :: Eq a => a -> Bool
ax_dequeue_enqueue_1 x = dequeue (enqueue x empty) == empty

ax_dequeue_enqueue_2 :: Eq a => a -> Queue a -> Property
ax_dequeue_enqueue_2 x q = not (isEmpty q) ==> dequeue (enqueue x q) == enqueue x (dequeue q)

-- Introducimos un sinónimo de tipo `T` para probar las propiedades
-- QuickCheck con tipos monomórficos.

type T = Integer -- o Char, Double, etc.

check_isEmpty_empty     = quickCheck (ax_isEmpty_empty :: Bool)
check_isEmpty_enqueue   = quickCheck (ax_isEmpty_enqueue :: T -> Queue T -> Bool)
check_first_enqueue_1   = quickCheck (ax_first_enqueue_1 :: T -> Bool)
check_first_enqueue_2   = quickCheck (ax_first_enqueue_2 :: T -> Queue T -> Property)
check_dequeue_enqueue_1 = quickCheck (ax_dequeue_enqueue_1 :: T -> Bool)
check_dequeue_enqueue_2 = quickCheck (ax_dequeue_enqueue_2 :: T -> Queue T -> Property)

-- Para comprobar todas las propiedades QuickCheck usamos `check_Queue`.

check_Queue = do
                 check_isEmpty_empty
                 check_isEmpty_enqueue
                 check_first_enqueue_1
                 check_first_enqueue_2
                 check_dequeue_enqueue_1
                 check_dequeue_enqueue_2
