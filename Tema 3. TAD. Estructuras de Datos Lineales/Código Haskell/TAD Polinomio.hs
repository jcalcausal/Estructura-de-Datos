----------------------------------------------------------------------------
-- Ángel Manuel Soria Gil - TAD Polinomio
----------------------------------------------------------------------------

module Polinomio
    (Polinomio
    , empty
    , grade
    , coeff
    , insert
    , remove
    , list2Pol
    , sumP
    , foldPol
    , eval
    ) where


import Test.QuickCheck
-- Invariante: Los polinomios se representan de forma ordenada de mayor a menor grado con todos los
-- coeficientes distintos de 0 y con todos los grados distintos

-- Implementación de Polinomio
type Grade = Int
type Coefficient = Integer

data Polinomio = Nil 
               | P Grade Coefficient (Polinomio) deriving Show

s1 = P 2 1 (P 1 3(P 0 8 Nil)) -- x^2 + 3x + 8
s2 = P 3 2 (P 1 2(P 0 10 Nil)) -- 2x^3 + 2x + 10

-- empty : Constructor de polinomio vacío
empty :: Polinomio
empty = Nil

-- grade : Devuelve el grado del polinomio, es decir, el grado del mayor coeficiente
grade :: Polinomio -> Grade
grade Nil = error "grade on nil polinomio"
grade (P g c ps) = g

-- coeff : Devuelve el coeficiente que acompaña a un grado determinado en un polinomio
coeff :: Grade -> Polinomio -> Coefficient
coeff _ Nil = error "coeff on nil polinomio or negative coefficient"
coeff wg (P g c ps) 
    | wg == g = c
    | wg >  g = error "coefficient not found on this polinomio"  
    | wg <  g = coeff wg ps

-- insert : suma un monomio a un polinomio dado
insert :: Grade -> Coefficient -> Polinomio -> Polinomio
insert g1 c1 Nil = P g1 c1 Nil
insert g1 c1 (P g2 c2 ps) 
    | g1 == g2 = P g1 (c2+c1) ps
    | g1 >  g2 = P g1 c1 (P g2 c2 ps)
    | g1 <  g2 = P g2 c2 (insert g1 c1 ps)

-- remove : elimina un grado determinado junto a su coeficiente de un polinomio
remove :: Grade -> Polinomio -> Polinomio
remove _ Nil = Nil
remove g1 (P g2 c2 ps) 
    | g1 == g2 = ps
    | g1 >  g2 = error "grade not found on this polinomio"
    | g1 <  g2 = P g2 c2 (remove g1 ps) 

-- list2pol : transforma una lista en un polinomio
list2Pol :: [Integer] -> Polinomio
list2Pol l = foldr (\(g, c) solResto-> insert g c solResto) Nil (zip [0..] l)

-- sumP : suma de polinomios
sumP :: Polinomio -> Polinomio -> Polinomio
sumP Nil Nil = Nil
sumP Nil (P g2 c2 ps2)           = P g2 c2 ps2
sumP (P g1 c1 ps1) Nil           = P g1 c1 ps1
sumP (P g1 c1 ps1) (P g2 c2 ps2) = sumP ps1 (insert g1 c1 (P g2 c2 ps2))

-- foldpol : plegado a derecha de polinomios
foldPol :: (Grade -> Coefficient -> b -> b) -> b -> Polinomio -> b
foldPol f solCasoBase pol = plegarPol pol
    where 
        plegarPol Nil = solCasoBase
        plegarPol (P c1 c2 ps) = f c1 c2 (plegarPol ps)

-- eval : evalua el polinomio en un valor dado
eval :: Integer -> Polinomio -> Integer
eval x pol = foldPol f 0 pol
    where f g c solResto = c*(x^g) + solResto