import Text.Show.Functions
import Data.List

data Festival = UnFestival {
                lugar :: String,
				animo :: String,
				publico :: Float,
				bandas :: [Banda]
				} deriving (Show)
				
hullabalooza :: Festival 
hullabalooza = UnFestival {
                lugar = "hullabalooza",
				animo = "indiferente",
				publico = 20000,
				bandas = [metallica,miranda,soda,losRedondos]
				} 
				
data Banda = UnaBanda{
             descripcion :: [String],
             decibeles :: Int,
             genero :: Genero
			 } deriving (Show)
			 
losRedondos :: Banda
losRedondos = UnaBanda{
             descripcion = ["legendaria","pogosa"],
             decibeles = 45,
             genero = rockNacional
			 }
			 
soda :: Banda
soda = UnaBanda{
             descripcion = ["irrepetible"],
             decibeles = 40,
             genero = rockNacional
			 }
			 
miranda :: Banda
miranda = UnaBanda{
             descripcion = ["insipida","incolora","inodora"],
             decibeles = 60,
             genero = pop
			 }
							 
			 
metallica :: Banda
metallica = UnaBanda{
             descripcion = ["legendaria","vendida"],
             decibeles = 60,
             genero = heavyMetal
			 }
			 
			 
				
type Genero = Festival -> Festival

rockNacional :: Genero				
rockNacional festival = aumentarPublico 100 festival

aumentarPublico valor festival = festival { publico = publico festival + valor}

pop :: Genero
pop festival = (cambiarAnimo.verSiAnimoEsIndiferente )festival

cambiarAnimo festival = festival { animo = "euforico"}

verSiAnimoEsIndiferente festival | animo festival == "indiferente" = festival { publico = publico festival * 2 }
                                 | otherwise = festival

heavyMetal :: Genero								 
heavyMetal festival = (agregarNombreAAnimo "pesado".aumentarPublico 0.01) festival

agregarNombreAAnimo nombre festival = festival {animo = animo festival ++ nombre }

trashMetal :: Genero
trashMetal festival = (agregarNombreAAnimo "basura".aumentarPublico 0.01) festival

--1 

tocar :: Banda -> Festival -> Festival
tocar banda festival = (genero banda) festival

--3

theStrokes ::Banda
theStrokes = UnaBanda{ 
            descripcion = ["suicidio asistido","emocional","linda"],
             decibeles = 45,
             genero = pop.heavyMetal
			 }

--4

suceder festival = foldr tocar festival (bandas festival)

--Main> suceder hullabalooza
--UnFestival {lugar = "hullabalooza", animo = "indiferentepesado", publico = 20000.01, bandas = [UnaBanda {descripcion = ["legendaria","vendida"], decibeles = 60, genero = <function>}]}

--5
type Criterios = Banda -> Bool

criterios:: [Criterios]
criterios = [vendida , acustica, legendaria]

vendida :: Criterios
vendida banda = tieneMasDeTresDescripciones banda

tieneMasDeTresDescripciones banda = (length (descripcion banda)) >= 3 || buscarDescripcion "vendida" banda

buscarDescripcion palabra banda =  elem palabra (descripcion banda)

acustica :: Criterios
acustica banda = tieneMasdecibeles 55 banda

tieneMasdecibeles valor banda = decibeles banda > valor

legendaria :: Criterios
legendaria banda = buscarDescripcion "legendaria" banda && tieneMasdecibeles 40 banda

--6

--popularidad banda criterios = map (algo banda) criterios 

--algo banda criterio = criterio banda


popularidad banda criterios = (length (filter (algo banda) criterios) ) *100

algo banda criterio = criterio banda
				
buenFest festival criterios = estaOrdenado festival criterios && sum (aplicarPopularidadACadaBanda festival criterios) > 1000

estaOrdenado festival criterios = sort (aplicarPopularidadACadaBanda festival criterios) == (aplicarPopularidadACadaBanda festival criterios)

aplicarPopularidadACadaBanda festival criterios = map (flip a criterios) (bandas festival)

a banda criterios = popularidad banda criterios
  
				
				