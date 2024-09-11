--Ejemplo como definir algo usando patrones

--Esta función nos dice donde está un punto de R2
estaEn :: (Int, Int) -> String
estaEn (0,0) = "en el origen"
estaEn (0,_) = "en el eje de de ordenadas"
estaEn (_,0) = "en el eje de abscisas"
estaEn (x,y) | x==y = "En la bisectriz"
             | otherwise = "en cualquier otro sitio"