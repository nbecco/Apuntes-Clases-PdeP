-APUNTES DE LA CLASE:
restar50 :: Number -> Number
restar50 = flip (-) 50

(+) ; (-) ; (*2) ; filter ; etc -> devuelve: <una funcion>



-COMPOSICION DE FUNCIONES:
f◦g = f( g(x) )
                   f           g       f(g(x))
composicion :: (b -> c) -> (a -> b) -> (a -> c)
composicion f g x = f (g x)

longitudEsPar1 = even . length
longitudEsPar2 x = even(length x)
Ambas funciones son iguales



-UTILIZACION DE LANDA λ:
map (\x -> x*x) [1,2,3,4,5]