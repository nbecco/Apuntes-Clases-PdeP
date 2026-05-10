module IntroFuncional where
import PdePreludat

doble :: Number -> Number
doble x = 2 * x

suma :: Number -> Number -> Number
suma nro1 nro2 = nro1 + nro2

longitud :: String -> Number
longitud palabra = length palabra

esGrande :: Number -> Bool
esGrande numero = numero > 100

esCopado :: Number -> Bool
esCopado numero = esGrande numero || even numero

esBuenisimo :: Number -> Bool
esBuenisimo numero = numero < 100 && esCopado numero

{- Ejercicio en Clase:
    Dada la edad de una persona, saber su costo de entrada al cine. 
    Si la edad es mayor a 18, el costo de edad *3. Si no lo es, el costo es 5.
-}
costoEntrada :: Number -> Number
costoEntrada edad
    | edad >= 18 = edad * 3
    | edad > 6 && edad < 18 = 90
    | otherwise = 5


{- Ejercicio en Clase:
    Dada una hora a la que se realiza una compra, quiero conocer el porcentaje
    de decuento que me realizan.
    Hasta las 10 hay descuento de 10%
    Despues de las 18 no hay descuento
    En cualquier otra hora, el descuento es de 5%
-}
descuento :: Number -> Number
descuento hora 
    | hora <= 10 = 10
    | hora >= 18 = 0
    | otherwise = 5


{- Ejercicio en Clase:
    Dado una palabra, calcular su nivel de rareza, que es el doble de su longitud.
    En particular, el nivel de rareza del nombre "Tom" es 900.
-}
rarezaDe :: String -> Number
rarezaDe nombre
    | nombre == "Tom" = 900
    | otherwise = doble (longitud nombre)

{- 
    Otra forma de escribir la función rarezaDe es utilizando pattern matching,
    que es una forma de definir funciones por casos, sin necesidad de utilizar
    guardas. Esta forma se llama Pattern Matching y se utiliza para definir 
    funciones que dependen de un valor 
-}
rareza2De :: String -> Number
rareza2De "Tom" = 900
rareza2De nombre = doble (longitud nombre)