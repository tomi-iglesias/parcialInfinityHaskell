import Text.Show.Functions()

main :: IO ()
main = return ()

type Derrota = (String, Int)
type Equipamiento = Personaje -> Personaje

data Personaje = CrearPersonaje {
    nombre :: String,
    cantidadDePoder :: Int,
    derrotas :: [Derrota],
    equipamiento :: [Equipamiento]
} deriving (Show)

capitanAmerica :: Personaje
capitanAmerica = CrearPersonaje "Capitan America" 501 [("Hijo de Thanos",1997)] []

thanos :: Personaje
thanos = CrearPersonaje "Thanos" 4100 [] [gemaDelAlma]

seLlama:: String -> Personaje -> Bool
seLlama unNombre unPersonaje = (==unNombre).nombre $ unPersonaje

obtenerNombreDerrota :: Derrota -> String
obtenerNombreDerrota unaDerrota = fst unaDerrota 

cambiarPoder :: Int -> Personaje -> Personaje
cambiarPoder unValor unPersonaje = unPersonaje {cantidadDePoder = cantidadDePoder unPersonaje + unValor}

agregarNombreAdelante :: String -> Personaje -> Personaje
agregarNombreAdelante unNombre unPersonaje = unPersonaje {nombre = unNombre ++ " " ++ nombre unPersonaje}

agregarVersionAlFinal :: Int -> Personaje -> Personaje
agregarVersionAlFinal unaVersion unPersonaje = unPersonaje {nombre = nombre unPersonaje ++ " V" ++ show unaVersion}

limpiarHistorialDerrotas :: Personaje -> Personaje
limpiarHistorialDerrotas unPersonaje = unPersonaje { derrotas = [] }

entrenamiento :: [Personaje] -> [Personaje]
entrenamiento unosPersonajes = map (multiplicarPoderes (length unosPersonajes)) unosPersonajes

multiplicarPoderes :: Int -> Personaje -> Personaje
multiplicarPoderes cantidadDePersonajes unPersonaje = unPersonaje { cantidadDePoder = cantidadDePoder unPersonaje * cantidadDePersonajes}

rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos unosPersonajes = filter esRivalDigno.entrenamiento $ unosPersonajes

esRivalDigno :: Personaje -> Bool
esRivalDigno unPersonaje = suPoderEsMayorA500 unPersonaje && fueDerrotadoPorHijoDeThanos unPersonaje

suPoderEsMayorA500 :: Personaje -> Bool
suPoderEsMayorA500 unPersonaje = cantidadDePoder unPersonaje > 500

fueDerrotadoPorHijoDeThanos :: Personaje -> Bool
fueDerrotadoPorHijoDeThanos unPersonaje = any (=="Hijo de Thanos").map obtenerNombreDerrota.derrotas $ unPersonaje

agregarDerrota :: Personaje -> String -> Int -> Personaje
agregarDerrota unPersonaje nombreDeLaDerrota anioDeLaGuerra = unPersonaje {derrotas = (nombreDeLaDerrota, anioDeLaGuerra) : derrotas unPersonaje} 

guerraCivil :: Int -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil anioDeLaGuerra unosPersonajes otrosPersonajes = map (pelear anioDeLaGuerra).zip unosPersonajes $ otrosPersonajes

pelear :: Int -> (Personaje,Personaje) -> Personaje
pelear anioDeLaGuerra (unPersonaje,otroPersonaje) 
    | cantidadDePoder unPersonaje > cantidadDePoder otroPersonaje = agregarDerrota unPersonaje (nombre otroPersonaje) anioDeLaGuerra
    | otherwise = agregarDerrota otroPersonaje (nombre unPersonaje) anioDeLaGuerra

escudo :: Equipamiento
escudo unPersonaje 
    | (length.derrotas $ unPersonaje) < 5 = cambiarPoder 50 unPersonaje
    | otherwise = cambiarPoder (-100) unPersonaje

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado unaVersion unPersonaje = agregarVersionAlFinal unaVersion.agregarNombreAdelante "Iron" $ unPersonaje

stormBreaker :: Equipamiento
stormBreaker unPersonaje 
    | seLlama "Thor" unPersonaje = agregarNombreAdelante "Dios del trueno".limpiarHistorialDerrotas $ unPersonaje
    | otherwise = unPersonaje

gemaDelAlma :: Equipamiento
gemaDelAlma unPersonaje
    | seLlama "Thanos" unPersonaje = agregarExtrasADerrotas unPersonaje
    | otherwise = unPersonaje

agregarExtrasADerrotas :: Equipamiento
agregarExtrasADerrotas unPersonaje = unPersonaje {derrotas = derrotas unPersonaje ++ crearListaDeExtras }

crearListaDeExtras :: [Derrota]
crearListaDeExtras = map agregarString (zip [1..] [2020..])

agregarString :: (Int,Int) -> Derrota
agregarString (unNumero,unAnio)= ("extra numero " ++ show unNumero, unAnio)

-- funcion hecha para que funcione codigo, no es necesario hacerla, ni la pide el enunciado. 
esGemaDelInfinito :: Equipamiento -> Bool
esGemaDelInfinito unaGema = show unaGema == show gemaDelAlma

guanteleteDelInfinto :: Equipamiento
guanteleteDelInfinto unPersonaje
    | seLlama "Thanos" unPersonaje = (aplicarEquipamientos.filter esGemaDelInfinito.equipamiento) unPersonaje $ unPersonaje 
    | otherwise = unPersonaje

aplicarEquipamientos :: [Equipamiento] -> Equipamiento
aplicarEquipamientos listaDeEquipamientos = foldl1 (.) listaDeEquipamientos

-- Parte C
{--No funcionaria, porque el escudo usa la funcion length de las derrotas, infinitas en este caso, y length espera a tener la lista completa
por lo tanto, se quedaria pensando 

En este desarrollo de mi codigo, no funcionaria, porque se quedaria iterando eternamente en el map de obtenerNombreDerrota.

Si se podria porque take 100 trabaja con la forma lazy evaluation, entonces una vez que tiene sus 100 derrotas, nos devolvera las mismas. 
--}