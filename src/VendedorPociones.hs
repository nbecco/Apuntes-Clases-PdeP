module VendedorPociones where
import PdePreludat

data Heroe = UnHeroe {
    vida :: Number,
    ataque :: Number,
    defensa :: Number
} deriving (Show, Eq)


--1)
caballero :: Heroe
caballero = UnHeroe {
    vida = 1000,
    ataque = 50,
    defensa = 20
}

pepa :: Heroe
pepa = UnHeroe {
    vida = 1001,
    ataque = 51,
    defensa = 21
}


--2)
poder :: Heroe -> Number
poder heroe = 3 * defensa heroe + ataque heroe + vida heroe / 2


--3)
type Modificacion = Number -> Number

--a.
cambiarVida :: Modificacion -> Heroe -> Heroe
cambiarVida modificacion heroe = heroe {
    vida = modificacion (vida heroe)
}

cambiarDefensa :: Modificacion -> Heroe -> Heroe
cambiarDefensa modificacion heroe = heroe {
    defensa = modificacion (defensa heroe)
}

cambiarAtaque :: Modificacion -> Heroe -> Heroe
cambiarAtaque modificacion heroe = heroe {
    ataque = modificacion (ataque heroe)
}

--b.
sumarVida :: Number -> Heroe -> Heroe 
sumarVida cantidad heroe = heroe {
    vida = vida heroe + cantidad
}


--4)
type Pocion = Heroe -> Heroe
--a. 
pocionBase :: Pocion
pocionBase heroe = heroe {
    vida = vida heroe + 10
}
pocionBase2 :: Pocion
pocionBase2 heroe = cambiarVida (+10) heroe

pocionBase3 :: Pocion
pocionBase3 = sumarVida 10


--b.
pocionPremium :: Pocion
pocionPremium = pocionBase3 . pocionBase3

pocionPremiumComposicion :: Pocion
pocionPremiumComposicion = pocionBase3 . pocionBase3


--c. 
crazyPotion :: Pocion
crazyPotion heroe = cambiarVida (* 1.33) (pocionPremium (cambiarAtaque (*2) heroe))

crazyPotionComposicion :: Pocion
crazyPotionComposicion = cambiarVida (* 1.33) . pocionPremium . cambiarAtaque (*2)


--d. 
agua :: Pocion
agua = id


--e. 
pocionElite :: Pocion
pocionElite heroe 
    | esPoderoso heroe = cambiarDefensa (* 10) heroe
    | otherwise = agua heroe

esPoderoso :: Heroe -> Bool
esPoderoso heroe = poder heroe > 1000


--f. 
pocionArtesanal :: Number -> Pocion
pocionArtesanal cantidad heroe = pocionBase3 (cambiarAtaque (/cantidad) (pocionBase3 heroe))

pocionArtesanalComposicion :: Number -> Pocion
pocionArtesanalComposicion cantidad = pocionBase3 . cambiarAtaque (/cantidad) . pocionBase3


--g. 
pocionArriesgada :: Pocion
pocionArriesgada heroe = pocionBase3 (cambiarDefensa (\x -> 3) (crazyPotion heroe))
pocionArriesgadaComposicion :: Pocion
pocionArriesgadaComposicion = pocionBase3 . cambiarDefensa (\x -> 3) . crazyPotion


--h. Esta la hice directamente con composición
licuadoPociones :: Pocion -> Pocion
licuadoPociones pocion = pocionArtesanal (10) . pocion . pocionBase3


--i.
pocionGradual :: Pocion
pocionGradual heroe 
    | ataque heroe > 100 = cambiarDefensa (+50) heroe
    | ataque heroe >= 50 && ataque heroe <= 100 = cambiarDefensa (+30) heroe
    | ataque heroe < 50 = cambiarDefensa (+10) heroe


--j.
pocionVampirica :: Pocion
pocionVampirica heroe 
    | defensa heroe > vida heroe = cambiarVida (+40) (cambiarDefensa (subtract 20) heroe)
    | otherwise = cambiarAtaque (+30) (cambiarVida (subtract 10) heroe)

pocionVampiricaComposicion :: Pocion
pocionVampiricaComposicion heroe 
    | defensa heroe > vida heroe = cambiarVida (+40) . cambiarDefensa (subtract 20) $ heroe -- PORQUE ME OBLIGA A PONER $ heroe?
    | otherwise = cambiarAtaque (+30) . cambiarVida (subtract 10) $ heroe