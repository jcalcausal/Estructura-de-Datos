------------------------------------------------------------
-- Estructuras de Datos
-- Tema 4. Árboles
-- Pablo López
--
-- Diccionarios en Haskell
------------------------------------------------------------

module DictDemos where

import           DataStructures.Dictionary.AVLDictionary

{-
   En la implementación del diccionario usamos dos novedades:

      1) constructores de datos infijos
      2) expresiones case
-}

-- Hasta ahora los constructores de datos han sido prefijos:

data Par a b = Par a b

p :: Par Int String
p = Par 3 "Java"

-- Un constructor de datos también puede ser infijo:

data Rel a b = a :-> b

r :: Rel Int String
r =  5 :-> "Haskell"

{-
   Hasta ahora la correspondencia de patrones está limitada
   a definiones de función
-}

ejemplo :: [a] -> String
ejemplo []      = "vacia"
ejemplo [_]     = "unitaria"
ejemplo (_:_:_) = "más de uno"

{-
   Las expresiones case son similares a la sentencia switch:
   se evalúa una expresión y se escoge el caso cuyo patrón
    encaja con el valor.
-}

ejemploCase :: [a] -> String
ejemploCase xs =
    case xs of  -- similar a switch
      []      -> "vacia"
      [_]     -> "unitaria"
      (_:_:_) -> "más de uno"

mkDict :: Ord a => [(a,b)] -> Dictionary a b
mkDict xs = foldr (\ (x,y) d -> insert x y d) empty xs

d :: Dictionary Integer String
d = mkDict [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
