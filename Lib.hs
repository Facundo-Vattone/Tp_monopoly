import Text.Show.Functions

data Jugador = Jugador {
    nombre :: String,
    dinero :: Int,
    tactica :: String,
    propiedades :: [Propiedad],
    accion :: [Accion]
}deriving Show

type Propiedad = (String, Int)
type Accion = Jugador -> Jugador

mapDinero :: (Int -> Int) -> Jugador -> Jugador 
mapDinero f jugador = jugador {dinero = f $ dinero jugador} 

mapTactica ::  (String -> String ) -> Jugador -> Jugador 
mapTactica f jugador = jugador {tactica = f $ tactica jugador} 

mapAccion :: ([Accion] -> [Accion] ) -> Jugador -> Jugador
mapAccion f jugador = jugador {accion = f $ accion jugador}

mapNombre :: (String -> String) ->  Jugador -> Jugador
mapNombre f jugador = jugador {nombre = f $ nombre jugador}

pasarPorElBanco :: Accion
pasarPorElBanco jugador = (mapDinero (+40)) . mapTactica  (const "comprador compulsivo ") $ jugador

--agregarAccion :: Accion -> Jugador -> Jugador
--agregarAccion unaAccion jugador = mapAccion (unaAccion : ) jugador

enojarse :: Accion 
enojarse jugador = mapDinero (+50) . mapAccion (gritar :)  $ jugador

gritar :: Accion
gritar jugador = mapNombre ("AHHHH " ++ ) jugador

mapPropiedades :: ([Propiedad] -> [Propiedad]) -> Jugador -> Jugador
mapPropiedades f jugador = jugador {propiedades = f $ propiedades jugador}

agregarPropiedad :: Propiedad -> Accion 
agregarPropiedad unaPropiedad jugador = mapPropiedades (unaPropiedad :) jugador

adquirirUnaPropiedad :: Propiedad -> Jugador -> Jugador
adquirirUnaPropiedad unaPropiedad jugador = mapDinero (subtract . snd $ unaPropiedad  )  . agregarPropiedad unaPropiedad $ jugador


subastar :: Propiedad -> Jugador -> Jugador 
subastar unaPropiedad jugador 
 | puedeAdquirir (tactica jugador) = adquirirUnaPropiedad unaPropiedad jugador
 | otherwise = id jugador

puedeAdquirir :: String -> Bool
puedeAdquirir "Accionista" = True
puedeAdquirir "Oferente singular" = True
puedeAdquirir _ = False


--sumarAlquileres :: Jugador -> Int                                                     
--sumarAlquileres  jugador = sum (map precioDelAlquiler (map snd (propiedades jugador))) 


sumarAlquileres :: Jugador -> Int                                                     
sumarAlquileres jugador  = sum.map precioDelAlquiler $ (map snd (propiedades jugador ))

precioDelAlquiler ::  Int -> Int
precioDelAlquiler unPrecio
 | unPrecio < 150 = 10
 | otherwise = 20 

cobrarAlquileres :: Accion
cobrarAlquileres jugador = mapDinero ((+).sumarAlquileres $ jugador) jugador

pagarAAccionistas :: Accion
pagarAAccionistas jugador 
 | "Accionista" == tactica jugador = mapDinero (+200) jugador
 | otherwise = mapDinero (subtract 100) jugador

hacerBerrinchePor :: Propiedad -> Jugador -> Jugador
hacerBerrinchePor unaPropiedad jugador 
 | dinero jugador >= snd unaPropiedad = adquirirUnaPropiedad unaPropiedad jugador
 | otherwise = hacerBerrinchePor unaPropiedad ((mapDinero (+10)). gritar $ jugador )

ultimaRonda :: Jugador -> Accion
ultimaRonda jugador = foldl1 (.) (accion jugador)

--ultimaRonda :: Accion
--ultimaRonda  jugador = (hacerTodasLasAcciones jugador) jugador



aplicarUltimaRonda :: Accion
aplicarUltimaRonda jugador = (ultimaRonda jugador) jugador

--empate :: Jugador -> Jugador -> [Jugador]
--empate unJugador otroJugador = [unJugador, otroJugador]

ganaElPrimero :: Jugador -> Jugador -> Bool
ganaElPrimero primero segundo = dinero primero > dinero segundo

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal unJugador otroJugador
 | ganaElPrimero (aplicarUltimaRonda unJugador) ( aplicarUltimaRonda otroJugador) = unJugador 
 | otherwise                                                                      = otroJugador


{-
juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal unJugador otroJugador 
 | dinero (ultimaRonda' unJugador) > dinero (ultimaRonda otroJugador) =   unJugador
 | dinero (ultimaRonda' unJugador) < dinero (ultimaRonda otroJugador) =  otroJugador
 | otherwise = unJugador -}





carolina = Jugador {
    nombre= "Carolina",
    dinero = 500,
    tactica = "Accionista",
    propiedades = [],
    accion = [pasarPorElBanco, pagarAAccionistas]
}

manuel = Jugador {
    nombre= "Manuel",
    dinero = 500,
    tactica = "Oferente singular",
    propiedades = [],
    accion = [pasarPorElBanco, enojarse]    
}







{- import Text.Show.Functions

data Jugador = Jugador {
    nombre :: String,
    dinero :: Int,
    tactica :: String,
    propiedades :: [Propiedad],
    accion :: [Accion]
}deriving Show

type Propiedad = (String, Int)
type Accion = Jugador -> Jugador

mapDinero :: (Int -> Int) -> Jugador -> Jugador 
mapDinero f jugador = jugador {dinero = f $ dinero jugador} 

mapTactica ::  Jugador -> Jugador 
mapTactica  jugador = jugador {tactica = "Comprador compulsivo"} 

mapAccion :: ([Accion] -> [Accion] ) -> Jugador -> Jugador
mapAccion f jugador = jugador {accion = f $ accion jugador}

mapNombre :: (String -> String) ->  Jugador -> Jugador
mapNombre f jugador = jugador {nombre = f $ nombre jugador}

pasarPorElBanco :: Accion
pasarPorElBanco jugador = (mapDinero (+40)) . mapTactica $ jugador

agregarAccion :: Accion -> Jugador -> Jugador
agregarAccion unaAccion jugador = mapAccion (unaAccion : ) jugador

enojarse :: Accion 
enojarse jugador = mapDinero (+50) . agregarAccion gritar $ jugador

gritar :: Accion
gritar jugador = mapNombre ("AHHHH " ++ ) jugador

mapPropiedades :: ([Propiedad] -> [Propiedad]) -> Jugador -> Jugador
mapPropiedades f jugador = jugador {propiedades = f $ propiedades jugador}

agregarPropiedad :: Propiedad -> Accion 
agregarPropiedad unaPropiedad jugador = mapPropiedades (unaPropiedad :) jugador

adquirirUnaPropiedad :: Propiedad -> Jugador -> Jugador
adquirirUnaPropiedad unaPropiedad jugador= mapDinero ( (+).(*(-1)).snd $ unaPropiedad  )  . agregarPropiedad unaPropiedad $ jugador


subastar :: Propiedad -> Jugador -> Jugador 
subastar unaPropiedad jugador 
 | (snd unaPropiedad <= dinero jugador) && ("Oferente singular" == tactica jugador || "Accionista" == tactica jugador) = adquirirUnaPropiedad unaPropiedad jugador
 | otherwise = id jugador

sumarAlquileres :: Jugador -> Int                                                     
sumarAlquileres  jugador = sum (map esBarataOCara (map snd (propiedades jugador))) 

esBarataOCara ::  Int -> Int
esBarataOCara unPrecio
 | unPrecio < 150 = 10
 | otherwise = 20 

cobrarAlquileres :: Accion
cobrarAlquileres jugador = mapDinero ((+).sumarAlquileres $ jugador) jugador

pagarAAccionistas :: Accion
pagarAAccionistas jugador 
 | "Accionista" == tactica jugador = mapDinero (+200) jugador
 | otherwise = mapDinero (+ (-100)) jugador

hacerBerrinchePor :: Propiedad -> Jugador -> Jugador
hacerBerrinchePor unaPropiedad jugador 
 | dinero jugador >= snd unaPropiedad = adquirirUnaPropiedad unaPropiedad jugador
 | otherwise = hacerBerrinchePor unaPropiedad ((mapDinero (+10)). gritar $ jugador )

hacerTodasLasAcciones :: Jugador -> (Jugador -> Jugador)
hacerTodasLasAcciones jugador = foldl1 (.) (accion jugador)

ultimaRonda :: Accion
ultimaRonda  jugador = (hacerTodasLasAcciones jugador) jugador

empate :: Jugador -> Jugador -> [Jugador]
empate unJugador otroJugador = [unJugador, otroJugador]

juegoFinal :: Jugador -> Jugador -> [Jugador]
juegoFinal unJugador otroJugador 
 | dinero (ultimaRonda unJugador) > dinero (ultimaRonda otroJugador) =   [ unJugador]
 | dinero (ultimaRonda unJugador) < dinero (ultimaRonda otroJugador) = [ otroJugador]
 | otherwise = empate unJugador otroJugador





carolina = Jugador {
    nombre= "Carolina",
    dinero = 500,
    tactica = "Accionista",
    propiedades = [],
    accion = [pasarPorElBanco, pagarAAccionistas]
}

manuel = Jugador {
    nombre= "Manuel",
    dinero = 500,
    tactica = "Oferente singular",
    propiedades = [],
    accion = [pasarPorElBanco, enojarse]    
}

-}
