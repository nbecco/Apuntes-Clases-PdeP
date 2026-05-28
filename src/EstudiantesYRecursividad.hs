module EstudiantesYRecursividad where
import PdePreludat

-- ESTE EJERCICIO VA DE LA MANO CON LOS APUNTES "Recursividad.md"

--1) 
data Estudiante = UnEstudiante{
    saludMental :: Number,
    conocimiento :: Number
} deriving Show

--a)
adquirirConocimiento :: Number -> Estudiante -> Estudiante
adquirirConocimiento cantidad estudiante = UnEstudiante{
    saludMental = saludMental estudiante,
    conocimiento = conocimiento estudiante + cantidad
}

adquirirConocimiento2 :: Number -> Estudiante -> Estudiante
adquirirConocimiento2 cantidad estudiante = estudiante{conocimiento = conocimiento estudiante + cantidad}

--b) 
quemarseElBocho :: Number -> Estudiante -> Estudiante
quemarseElBocho cantidad estudiante = estudiante{saludMental = saludMental estudiante - cantidad}


--2) 
--a)
primerizo :: Estudiante
primerizo = UnEstudiante{
    saludMental = 150,
    conocimiento = 0
}

--b) 
recursante :: Estudiante
recursante = UnEstudiante{
    saludMental = 75,
    conocimiento = 70
}


--3)
data Tema = UnTema{
    dificultad :: Number,
    aprendizaje :: Number
} deriving Show

--a)
expresividad :: Tema
expresividad = UnTema{
    dificultad = 30,
    aprendizaje = 100
}

--b)
declaratividad :: Tema
declaratividad = UnTema{
    dificultad = 30,
    aprendizaje = 150
}

--c)
recursividad :: Tema
recursividad = UnTema{
    dificultad = 50,
    aprendizaje = 40
}


--4) REHACER PORQUE NO ENTENDI NADA ??
type Modificador = Number -> Number
estudiar :: Modificador -> Modificador -> Tema -> Estudiante -> Estudiante
estudiar modificadorConocimiento modificadorSaludMental tema = 
    adquirirConocimiento((modificadorConocimiento . aprendizaje) tema) . 
    quemarseElBocho ((modificadorSaludMental . dificultad) tema)

--b)
aprenderTema :: Modificador -> Tema -> Estudiante -> Estudiante
aprenderTema modificadorConocimiento = adquirirConocimiento . modificadorConocimiento . aprendizaje
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

--SEGUN YO
-- aprenderTema :: (Number -> Number) -> (Tema -> Number) -> (Number -> (Estudiante -> Estudiante))
-- estresare :: (Number -> Number) -> (Tema -> Number) -> (Number -> (Estudiante -> Estudiante))

--SEGUN LA PROFE
-- aprenderTema :: (Number -> (Estudiante -> Estudiante)) -> (Tema -> Number) -> (Number -> (Estudiante -> Estudiante))

--a)
estresarse :: Modificador -> Tema -> Estudiante -> Estudiante
estresarse modificadorSaludMental = quemarseElBocho . modificadorSaludMental . dificultad


--5) Va de la mano con el punto 4 pero lo entendi este
type MetodoDeEstudio = Tema -> Estudiante -> Estudiante

--a)
llevarlaAlDia :: MetodoDeEstudio
llevarlaAlDia = estudiar id id

--b)
sinDormir :: MetodoDeEstudio
sinDormir = estudiar (*(2/3)) (*2)

--c)
chatGPT :: MetodoDeEstudio
chatGPT = estudiar (*(1/3)) (*0.5)


--6) Entendi pero nunca en la vida podria hacerlo por mi cuenta
type Sesion = [Tema]
hacerSesionDeEstudio :: MetodoDeEstudio -> Estudiante -> Sesion -> Estudiante
hacerSesionDeEstudio _ estudiante [] = estudiante
hacerSesionDeEstudio metodo estudiante (tema:restoTemas) = 
    hacerSesionDeEstudio metodo (metodo tema estudiante) restoTemas


--7) 
--a) 
maratonDeExpresividad :: Sesion
maratonDeExpresividad = repeat expresividad

--b) 
-- PENSADO POR MI
maratonIntercalada :: Sesion
maratonIntercalada = concat (repeat [declaratividad, recursividad])

-- PENSADO POR LA PROFE
maratonIntercalada' :: Sesion
maratonIntercalada' = cycle [declaratividad, recursividad]