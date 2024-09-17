{-
 - Ejercicios Relación 1.
 - Estructura de Datos. 
 - Doble Grado Matemáticas + Ingeniería Informática.
 - Ángel Manuel Soria Gil
-}

import Test.QuickCheck
------------------------------------------------------------------------------------------------------
-- Ejercicio 3. Conversor de horas
------------------------------------------------------------------------------------------------------
-- tipos definidos para el ejercicios
type TotalSegundos  = Integer
type Horas          = Integer
type Minutos        = Integer
type Segundos       = Integer
-- función anterior necesaria (estudia si un valor se encuentra en un intervalo cerrado)
entre :: (Ord a) => a -> (a, a) -> Bool
entre x (s, t) = s<=x && x<=t

-- función que descompone un total de segundos en horas, minutos y segundos.
descomponer :: TotalSegundos -> (Horas, Minutos, Segundos)
descomponer x = (a, b, c)
    where 
        a = x `div` 3600
        b = (x `mod` 3600) `div` 60
        c = (x `mod` 3600) `mod` 60

-- propiedad de la función descomponer
p1_descomponer x = x>=0 ==> h*3600 + m*60 + s == x 
                            && entre m (0, 59)
                            && entre s (0, 59)
                                where (h, m, s) = descomponer x

