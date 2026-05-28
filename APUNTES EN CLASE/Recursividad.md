APUNTES DE LA CLASE:

- INTRODUCCION DE RECURSIVIDAD:
factorial :: Number -> Number
factorial 0 = 1         // Esto se pone para que corte porque sino el codigo rompe
factorial n = n - factorial (n-1)   // Es importante que esta linea este por debajo del pattern matching 
                                       ya que sino el pattern matching no tendria sentido

map' :: (a -> b) -> [a] -> [b]
map' transformador [] = 
map' transformador (x:xs) = transformador x : map' transformador xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' condicion (x:xs) = condicion x || any' condicion xs



- TIP SOBRE GUARDAS: no se usan guardas cuando se esta devolviendo un Booleano, porque es redundante



- INTRODUCCION FUNCION FOLD
Ejemplo de la hoja doblada del 0 al 7 y se van sumando de a dos (0+1=1+2=3+3=6+... hasta 28)

:t foldl    // La l viene de left
foldl :: (b -> a -> b) -> b -> [a] -> b
ej: foldl (+) 1 [1,2,3,4,5] = 14 

:t foldr    // La r viene de rifht
foldr :: (a -> b -> b) -> b -> [a] -> b
ej: foldr (-) 1 [1,2,3,4,5] = 2

- Diferencia entre foldl y foldr:
> foldl (-) 1 [1,2,3,4,5]
> -14

> foldr (-) 1 [1,2,3,4,5]
> 2

- foldl1 y foldr1: hacen lo mismo que las otras solo que no hace falta pasarle un parametro



- LAZY EVALUATION vs. EAGER EVAULATION
Damos un ejemplo con tuplas para que se entienda:
primero (x, y) = x

ej: eager evaluation (evaluacion ansiosa)
primero(1+2, 3+2)
primero (3, 5)
3

ej: lazy evaluation (evaluacion diferida)
primero (1+2, 3+2)
1+2
3

En conlusion, se ve claramente como nos ahorramos una ejecucion con el lazy evaluation

IMPORTANTISIMO: HASKELL SIEMPRE USA LAZY EVALUATION (ESTA JUSTIFICACION SE UTILIZA EN PARCIALES)