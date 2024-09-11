-------------------------------------------------------------------------------------------
-- Ángel Manuel Soria Gil 
-- TAD Dock
-------------------------------------------------------------------------------------------

module Dock
    (Dock
--    , empty
--    , isEmpty
--    , sign
--    , isFirst
--    , isLast
--    , left
 --   , right
--    , delete
--    , insertl
--    , insertr
--    , listToDock 
    ) where 

import qualified Data.List as L
import qualified DataStructures.Stack.LinearStack as S
import Test.QuickCheck

--Implementación TAD Dock 
data Dock a = D (S.Stack a) (S.Stack a) deriving Eq

--Ejemplo 
ex1 = D (S.push 1 (S.push 3 (S.push 4 S.empty))) (S.push 5 (S.push 7 (S.push 2 S.empty)))
ex2 = D (S.empty) (S.push 5 (S.push 7 (S.push 2 S.empty)))
ex3 = D (S.empty) (S.push 5 S.empty)

-- empty : crea un dock vacio
empty :: Dock a
empty = D (S.empty) (S.empty)

-- isEmpty : bool que devuelve si nuestro dock es vacio
isEmpty :: Dock a -> Bool
isEmpty (D s1 s2) = S.isEmpty s1 && S.isEmpty s2

-- sign : funcion que devuelve el elemento destacado del dock, es decir, devuelve 
-- el primer elemento de la segunda stack
sign :: Dock a -> a 
sign (D s1 s2) = S.top s2

-- isFirst : función que devuelve si el elemento destacado del dock es el primero
isFirst :: Dock a -> Bool
isFirst (D s1 s2) 
    | isEmpty (D s1 s2)                  = error "applying isFirst on empty Dock"
    | S.isEmpty s1 && not (S.isEmpty s2) = True 
    | otherwise                          = False

-- isLast : funcion que devuelve si el elemento destacado del doc es el último
isLast :: Dock a -> Bool
isLast (D s1 s2) 
    | isEmpty (D s1 s2)                                            = error "aplying isLast on empty Dock"
    | S.isEmpty s1 && not (S.isEmpty s2) && (S.isEmpty (S.pop s2)) = True
    | otherwise                                                    = False

-- left : funcion que cambia el elemento destacado al de la izquierda
left :: Dock a -> Dock a 
left (D s1 s2) = D (S.pop s1) (S.push (S.top s1) s2)

-- right : funcion que cambia el elemento destacado al de la derecha
right :: Dock a -> Dock a 
right (D s1 s2) = D (S.push (S.top s2) s1) (S.pop s2)

-- delete : borra el elemento destacado 
delete :: Dock a -> Dock a 
delete (D s1 s2) = D s1 (S.pop s2) 

-- insertl : añade un elemento a la izquierda del destacado
insertl :: a -> Dock a -> Dock a 
insertl elem (D s1 s2) = D (S.push elem s1) s2 

-- insertr : añade un elemento a la derecha del destacado
insertr :: a -> Dock a -> Dock a
insertr elem (D s1 s2) = D s1 (S.push (S.top s2) (S.push elem (S.pop s2)))

-- listToDock : transforma una lista en un Dock, como no se especifica nada, tomaremos el primero
-- como el destacado
listToDock :: [a] -> Dock a
listToDock (x:xs) = foldr f (D s1 s2) (xs)
    where 
        f a solResto = insertr a solResto
        s2 = S.push x S.empty
        s1 = S.empty


-- Showing for Dock 
instance (Show a) => Show (Dock a) where 
    show (D s1 s2) = "TwoStackDock"++" ["++(show s1)++"<"++( show (S.top s2))++">"++ show (S.pop s2)++"]"