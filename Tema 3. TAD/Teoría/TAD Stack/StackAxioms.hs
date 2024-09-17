------------------------------------------------------------
-- Estructuras de Datos
-- Tema 3. Estructuras de Datos Lineales
-- Pablo López
--
-- Módulo de la especificación del TAD Pila
------------------------------------------------------------

module StackAxioms where

--import           DataStructures.Stack.LinearStack
--import           DataStructures.Stack.StackOnLis
import           Stack

import           Test.QuickCheck

-- | Especificación del TAD Stack
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

   Para el TAD Pila tenemos dos constructores: `push` y `empty`. El
   resto de operaciones son:

        - selectores (`isEmpty`, `top`)
        - transformadores (`pop`).

   Por ejemplo, para especificar la operación `isEmpty` tenemos
   que especificar qué devuelve `isEmpty` cuando se aplica a una Pila
   construida con `empty` o con `push`, etc.:

         isEmpty empty = ???
         isEmpty (push x s) = ???

         pop (push x s) = ???

         top (push x s) = ???

   Cada una de las líneas anteriores es un axioma de la especificación formal.
   ¿Por qué no hay axiomas para `pop` y `top` sobre `empty`?
   ¿Qué implican los axiomas sobre `pop` y `top`?

   Cada axioma de la especificación formal lo representaremos mediante
   un propiedad QuickCheck. La ventaja de usar propiedades QuickCheck es
   que podremos comprobar si todas nuestras implementaciones de la Pila
   son correctas: una implementación es correcta si satisface todos los
   axiomas de la definición formal.

-}

ax_isEmpty_empty :: Bool
ax_isEmpty_empty = isEmpty empty == True

ax_isEmpty_push :: a -> Stack a -> Bool
ax_isEmpty_push x s = isEmpty (push x s) == False

{-
   ¿Qué devuelve `top` cuando se aplica a un constructor?
-}

ax_top_push :: Eq a => a -> Stack a -> Bool
ax_top_push x s = top (push x s) == x

{-
   Observa que `top` sobre `empty` no está definido y, por lo tanto,
   no se define. Sería como definir qué devuelven la división por
   cero, el logaritmo de un valor negativo o un array al que se aplica
   un índice fuera de rango. La función `top` es parcial.

   Sin embargo, la implementación algo tendrá que hacer, porque es
   posible que durante la ejecución se trate de evaluar algo que no
   está definido. Lo más habitual en los lenguajes modernos es elevar
   una excepción. En cualquier caso, es una cuestión que depende de la
   implementación, no de la definición.

   ¿Qué devuelve `pop` cuando se aplica a un constructor?
-}

ax_pop_push :: Eq a => a -> Stack a -> Bool
ax_pop_push x s = pop (push x s) == s

{-
   Las anteriores propiedades QuickCheck son polimórficas: para poder
   usarlas debemos proporcionar un tipo concreto. Lo que haremos será
   introducir un sinónimo de tipo `T` para algún tipo como `Integer`,
   `Char`, etc.
-}

type T = Integer -- Char, String, etc.

{-
   Esto nos permite comprobar las propiedades para el tipo
   `T`. Cambiando la definición de `T`, podemos probar con diferentes
   tipos.
-}

check_isEmpty_empty = quickCheck (ax_isEmpty_empty :: Bool)
check_isEmpty_push  = quickCheck (ax_isEmpty_push :: T -> Stack T -> Bool)
check_pop_push      = quickCheck (ax_pop_push :: T -> Stack T -> Bool)
check_top_push      = quickCheck (ax_top_push :: T -> Stack T -> Bool)

{-
   Para comprobar todas las propiedades QuickCheck podemos definir una
   función que invoque todas las propiedades en un bloque `do`:
-}

check_Stack = do
                 check_isEmpty_empty
                 check_isEmpty_push
                 check_pop_push
                 check_top_push