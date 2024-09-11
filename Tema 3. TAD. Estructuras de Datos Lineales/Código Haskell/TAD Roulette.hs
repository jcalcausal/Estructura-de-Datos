-----------------------------------------------------------------------------
-- Ángel Manuel Soria Gil -- TAD Ruleta
-----------------------------------------------------------------------------

module Roulette
    ( Roulette
    , empty
    , isEmpty
    , sign
    , turn
    , delete
    , insert
    , mapRoulette
    , listToRoulette
    , rouletteToList
    ) where

import Data.List(intercalate) 
import qualified DataStructures.Queue.LinearQueue as Q
import Test.QuickCheck

-- Implementación Roulette 
data Roulette a = R (Q.Queue a) Integer deriving Eq

-- Un ejemplo
sample1 = R (foldl (flip Q.enqueue) Q.empty [3,4,5,6,7,8,9,10,1,2]) 10

-- Constructor de una ruleta vacía
empty :: Roulette a 
empty = R(Q.empty) 0

-- isEmpty devuelve un Bool indicando si es vacía o no la ruleta
isEmpty :: Roulette a -> Bool
isEmpty (R cola tam) = tam == 0

-- sign devuelve el valor del tipo a que destaca la ruleta
sign :: Roulette a -> a
sign (R cola tam) = Q.first cola

-- turn: gira la ruleta un número de veces en sentido horario si el número es 
-- positivo y en antihorario si es negativo
turn :: Integer -> Roulette a -> Roulette a
turn n (R cola tam) 
    | tam == 0 ||n == 0 || mod n tam == 0 = (R cola tam) 
    | n < 0 = turn (n+1) (R (Q.dequeue(Q.enqueue (Q.first cola) cola)) tam)
    | n > 0 = turn (n-tam) (R cola tam)

-- delete : elimina el elemento destacado, es decir, el primero y el destacado pasaa a ser el siguiente
delete :: Roulette a -> Roulette a 
delete (R cola tam) = R (Q.dequeue cola) (tam-1)

-- insert : añade un elemento delante del destacado y este pasa a ser el destacado
insert :: a -> Roulette a -> Roulette a 
insert elem (R cola tam) = turn 1 (R (Q.enqueue elem cola) (tam+1)) 

-- mapRoulette : función que aplica una función a todos los elementos de la ruleta
-- consiguiendo una nueva ruleta 
mapRoulette:: (a->b) -> Roulette a -> Roulette b
mapRoulette f (R cola tam)
    | isEmpty (R cola tam)   = empty
    | otherwise = (f (Q.first cola)) `insert` mapRoulette f (R (Q.dequeue cola) (tam-1)) 

-- listToRoulette : función que transforma una lista en una ruleta
listToRoulette :: [a] -> Roulette a
listToRoulette (x:xs) = foldr f empty (x:xs)
    where f elem solResto = insert elem solResto

-- RouletteToList : función que transforma una ruleta en una lista
rouletteToList :: Roulette a -> [a]
rouletteToList (R cola tam)
    | isEmpty (R cola tam) = []
    | otherwise = (Q.first cola) : solResto 
        where solResto = rouletteToList (R (Q.dequeue cola) (tam-1))

-- Axioma de Roulette : Girar a la derecha y luego a la izquierda no afecta
p1_axiom_turn :: Eq a => Integer -> Roulette a -> Bool
p1_axiom_turn n r = turn n (turn (-n) r) == turn (-n) (turn n r)

-- Showing a roulette
instance (Show a) => Show (Roulette a) where
    show (R q size) = "QueueRoulette:"++show size++"(" ++ (intercalate "," (aux q)) ++ ")"
        where 
            aux q1 
                | Q.isEmpty q1 = []
                | otherwise = show x :aux q'
                    where
                         x = Q.first q1 
                         q' = Q.dequeue q1
                
-- Arbitrary instance
instance Arbitrary a => Arbitrary (Roulette a) where
    arbitrary = do
        xs <- listOf arbitrary
        return (foldr insert empty xs)