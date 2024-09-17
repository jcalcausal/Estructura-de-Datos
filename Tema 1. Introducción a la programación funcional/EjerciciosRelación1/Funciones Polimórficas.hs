{-
 - Ejercicios Relación 1.
 - Estructura de Datos. 
 - Doble Grado Matemáticas + Ingeniería Informática.
 - Ángel Manuel Soria Gil
-}

import Test.QuickCheck
------------------------------------------------------------------------------------------------------
-- Ejercicio 2. Funciones polimórficas
------------------------------------------------------------------------------------------------------
-- función que intercambia dos elementos en una dupla
intercambia :: (a, b) -> (b, a)
intercambia (x, y) = (y, x)

-- función que ordena una dupla de menor a mayor 
ordena2 :: (Ord a) => (a, a) -> (a, a)
ordena2 (x, y) = ((min x y), (max x y))

-- propiedaddes de la función ordena2
p1_ordena2 x y = enOrden (ordena2 (x, y))
    where enOrden (a, b) = a<=b

p2_ordena2 x y = mismosElementos (x, y) (ordena2 (x, y))
    where mismosElementos (x, y) (s, t) = (x==s && y==t) || (x==t && y==s)

-- función que ordena una tupla de tres elementos de menor a mayor
ordena3 :: (Ord a) => (a, a, a) -> (a, a, a)
ordena3 (x, y, z)
    | x<=y && y<=z = (x, y, z)
    | x<=y && x<=z = (x, z, y)
    | y<=x && x<=z = (y, x, z)
    | y<=x && y<=z = (y, z, x)
    | z<=x && x<=y = (z, x, y)
    | z<=x && z<=y = (z, y, z)

-- propiedades de la función ordena3
p1_ordena3 x y z = enOrden3 (ordena3 (x, y, z))
    where enOrden3 (a, b, c) = a<=b && b<=c

p2_ordena3 x y z = mismosElementos3 (x, y, z) (ordena3 (x, y, z))
    where mismosElementos3 (a, b, c) (r, s, t) = (a==r && b==s && c==t) || (a==r && b==t && c==s) || (a==s && b==r && c==t) || (a==s && b==t && c==r) || (a==t && b==r && c==s) || (a==t && b==s && c==r)

-- función que devuelve el máximo de dos elementos
max2 :: (Ord a) => a -> a -> a
max2 x y 
    | x<=y = y
    | otherwise = x

--propiedades de la función max2
p1_max2 x y = esUnoDeEllos x y
    where esUnoDeEllos x y = (max2 x y) == x || (max2 x y) == y

p2_max2 x y = esMayorOIgualqueEllos x y
    where esMayorOIgualqueEllos x y = (max2 x y) >= x && (max2 x y) >= y

p3_max2 x y = if x>=y then (max2 x y) == x else (max2 x y) == y

-- función que estudia si un valor se encuentra en un intervalo cerrado
entre :: (Ord a) => a -> (a, a) -> Bool
entre x (s, t) = s<=x && x<=t

-- función que devuelve si en una terna los tres valores son iguales
iguales3 :: (Eq a) => (a, a, a) -> Bool
iguales3 (x, y, z) = x==y && y==z
