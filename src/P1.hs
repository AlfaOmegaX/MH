-- Autor: Miguel Lentisco Ballesteros
-- Metaheurísticas P1 : APC ~ Relief y búsqueda local

module Main where

{-# LANGUAGE StrictData, Strict #-}
import qualified Data.Vector.Unboxed as V
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Maybe as M
import System.Random.Shuffle
import System.Directory
import System.Environment
import System.FilePath.Posix(takeBaseName)
import System.Random
import System.Clock
import Control.DeepSeq
import Control.Monad.State
import Data.Random.Normal
--import Debug.Trace

-- Una clase se puede representar como texto
type Clase = String
-- Un punto es un vector real
type Punto = V.Vector Float
-- Un dato es un vector de n caracteristicas reales (el punto) junto a su clase
type Dato = (Punto, Clase)
-- Una conjunto de datos
type Datos = [Dato]
-- Una partición esta compuesta de un conjunto de entramiento y uno de prueba
type Particion = (Datos, Datos)
-- Un conjunto de particiones
type Particiones = [Particion]
-- Unos pesos son un vector de n valores reales entre [0,1]
type Pesos = V.Vector Float
-- Un algoritmo es una función que lee unos datos (conjunto entrenamiento) y da una solución (pesos)
type Algoritmo = Datos -> Pesos
-- Lista de algoritmos
algoritmos :: StdGen -> [(String, Algoritmo)]
algoritmos gen = [("Pesos Uno", pesosUno), ("Relief", relief), ("Pesos aleatorios", pesosRand gen), ("Busqueda local", busLoc gen)]
-- Estructura de datos para encapsular una solución en la búsqueda local
data Solucion = Solucion {
  getPesos :: Pesos, -- La solución propiamente dicha (los pesos)
  getFit :: Float, -- El resultado de evaluar con la función objetivo los pesos de la solución
  getNVecinos :: Int -- El nº de vecinos obtenidos
}
-- Crea un objeto Solucion a partir de unos datos y unos pesos
crearSolucion :: Datos -> Pesos -> Solucion
crearSolucion datos pesos = Solucion pesos fit 0
  where fit = evaluarF datos pesos
-- Aumenta en un uno el nº de vecinos de una solución
aumentaVecino :: Solucion -> Solucion
aumentaVecino sol = Solucion (getPesos sol) (getFit sol) (getNVecinos sol + 1)
-- Definición de un evaulador de estados, con estado un generador
-- Esto es para obtener valores aleatorios
type Rand a = State StdGen a

---------------------------------------------------------------------------------
-- Funciones generales
---------------------------------------------------------------------------------
-- Devuelve el nº de características que tiene un conjunto de datos
nCaract :: Datos -> Int
nCaract ((punto, _):_) = V.length punto

-- Función objetivo
fEvaluacion :: Float -> Float -> Float -> Float
fEvaluacion alpha tAcier tRed = alpha * tAcier + (1 - alpha) * tRed
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Lectura de tipo .arff, normalización y división según clase
---------------------------------------------------------------------------------
-- Crea un dato a partir de una linea del fichero
creaDato :: String -> Dato
creaDato linea = (V.fromList $ fmap read $ init vector, filter (/=' ') $ last vector)
  where vector = S.splitOn "," linea

-- Lee el archivo y crea una lista con los datos normalizados
crearDatos :: String -> Datos
crearDatos contenido = L.nub $ fmap creaDato rawDatos
    where (_:rawDatos) = dropWhile (/= "@data") $ lines contenido

-- Lee los datos sin normalizar y los normaliza entre [0,1]
normalizar :: Datos -> Datos
normalizar datos =
  let (first:_)    = datos
      maxMin x y   = (fst x `max` y, snd x `min` y)
      inicial      = V.map (\x -> (x, x)) $ fst first
      lMaxMin      = foldl (\acc x -> V.zipWith maxMin acc (fst x)) inicial datos
      maxI         = fst . (lMaxMin V.!)
      minI         = snd . (lMaxMin V.!)
      dif i        = maxI i - minI i
      norValor i x = if dif i == 0 then 0.0 else (x - minI i) / dif i
  in fmap (\x -> (V.imap norValor (fst x), snd x)) datos

-- Divide los datos según su clase
separarClases :: Datos -> [Datos]
separarClases datos = L.groupBy (\(_, c1) (_, c2) -> c1 == c2) datosOrdenados
  where datosOrdenados = L.sortBy (\(_, c1) (_, c2) -> compare c1 c2) datos
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- k-fold cross-validation
---------------------------------------------------------------------------------
-- Crea las tuplas de particiones con (ConjuntoEntrenamiento, ConjuntoTest)
eleccionCruzada :: StdGen -> Int -> [Datos] -> Particiones
eleccionCruzada gen k datos =
  let parts = crearParticiones gen k datos
  in  foldl (\acc x -> acc ++ [(concat (L.delete x parts), x)]) [] parts

-- pre: nº datos de cada clase > k
-- Toma las distintas listas de datos de clases y forma las k particiones
crearParticiones :: StdGen -> Int -> [Datos] -> [Datos]
crearParticiones gen k datos =
  let (particiones, resto) = foldl (uneParticiones k) (replicate k [], []) datos
      restoMezclado = if resto == [] then [] else shuffle' resto (length resto) gen
      (particiones', resto') = if length restoMezclado < k then (particiones, restoMezclado) else uneParticiones k (particiones, []) restoMezclado
  in mergeResto resto' particiones'

-- Junta todas las k particiones de cada clase y una lista de todos los restos
uneParticiones :: Int -> ([Datos], Datos) -> Datos -> ([Datos], Datos)
uneParticiones k (acc1, acc2) listaClase = (zipWith L.union acc1 particiones, L.union acc2 resto)
  where (particiones, resto) = divideClases k listaClase

-- Divide la lista de una clase en k particiones y el resto
divideClases :: Int -> Datos -> ([Datos], Datos)
divideClases k listaClase = if length particiones == k then (particiones, []) else (init particiones, last particiones)
  where particiones = S.chunksOf (length listaClase `quot` k) listaClase

-- Reparte uniformemente los restos de hacer particiones
mergeResto :: Datos -> [Datos] -> [Datos]
mergeResto [] xs = xs
mergeResto (x:lResto) (y:parts) = (y ++ [x]) : mergeResto lResto parts
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- 1-NN
---------------------------------------------------------------------------------
-- pre: los pesos ya están reducidos
-- Clasificador 1-nn con pesos, devuelve el porcentaje de acierto
clas1nn :: Datos -> Datos -> Pesos -> Float
clas1nn lTrain lTest pesos =
  let distClase p        = fmap (\x -> (dist2P x p pesos, snd x)) $ (lTrain L.\\ [p])
      distOrd p          = L.sortBy (\(x,_) (y,_) -> compare x y) $ distClase p
      aciertoClase punto = snd punto == (snd $ head $ distOrd punto)
      aciertos           = fmap aciertoClase lTest
  in (L.genericLength $ filter (== True) aciertos) / L.genericLength aciertos

-- Distancia euclidea considerando pesos
dist2P :: Dato -> Dato -> Pesos -> Float
dist2P (p1,_) (p2,_) pesos = V.sum $ V.zipWith3 (\x y z -> z * (y - x) * (y - x)) p1 p2 pesos
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Evaluación de algoritmos
---------------------------------------------------------------------------------
-- Ejecuta todos los algoritmos que se tienen y devuelve el resultado de todos
ejecutarAlgoritmos :: Particiones -> StdGen -> IO String
ejecutarAlgoritmos particiones gen = do
    resultados <- mapM (ejecuta particiones) (algoritmos gen)
    return $ init $ init $ unlines resultados

-- Ejecuta los algoritmos en cada partición y devuelve un mensaje con todos los resultados
ejecuta :: Particiones -> (String, Algoritmo) -> IO String
ejecuta particiones (nomAlg, alg) =
  do
    resultados <- mapM (obtenerResultados alg) particiones
    let nombreAlgoritmo             = "Algoritmo " ++ nomAlg ++ ":\n"
    let (tAci, tRed, t, n, mensaje) = foldl crearInfo (0.0, 0.0, 0.0, 0, nombreAlgoritmo) resultados
    let aciMedio                    = tAci / fromIntegral n
    let redMedio                    = tRed / fromIntegral n
    let tMedio                      = t / fromIntegral n
    return $ mensaje ++ "Acierto medio: " ++ show aciMedio ++ " %, reduccion media: " ++ show redMedio ++ " %, evalucion media: " ++ show (fEvaluacion 0.5 aciMedio redMedio) ++ "%, tiempo medio: " ++ show tMedio ++ "s\n"

-- Formateo de mensaje de salida
crearInfo :: (Float, Float, Float, Int, String) -> (Float, Float, Float) -> (Float, Float, Float, Int, String)
crearInfo (a, r, t, i, c)  (x, y, z) =
    let mensaje = "Particion " ++ show (i + 1) ++ ": Acierto " ++ show x ++ " %, reduccion: " ++ show y ++ "%, evaluacion: " ++ show (fEvaluacion 0.5 x y) ++  "%, tiempo: " ++ show z ++ "s\n"
    in (a + x, r + y, t + z, i + 1, c ++ mensaje)

-- Aplica un algoritmo para obtener pesos y devuelve los resultados junto con el tiempo tardado
obtenerResultados :: Algoritmo -> Particion -> IO (Float, Float, Float)
obtenerResultados alg (train, test) =
  do
    (pesos, tiempo) <- pesosTiempo alg train
    let pReduccion =  (fromIntegral $ V.length $ V.filter (< 0.2) pesos) / (fromIntegral $ V.length pesos)
    let pAcierto = clas1nn train test $ reducePesos pesos
    return (100.0 * pAcierto, 100.0 * pReduccion, tiempo)

-- Aplica el algoritmo para obtener pesos y devuelve los pesos y el tiempo tardado
pesosTiempo :: Algoritmo -> Datos -> IO (Pesos, Float)
pesosTiempo alg train =
  do
    t1 <- getTime Realtime
    let pesos = alg train
    t2 <- pesos `deepseq` (getTime Realtime)
    return (pesos, fromIntegral(toNanoSecs(t2 - t1)) / 1000000000)

-- Reduce los pesos (pone a 0 los pesos que valgan menos de 0.2)
reducePesos :: Pesos -> Pesos
reducePesos = V.map (\x -> if x < 0.2 then 0.0 else x)
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Pesos aleatorios
---------------------------------------------------------------------------------
pesosRand :: StdGen -> Algoritmo
pesosRand gen datos = V.fromList $ take (nCaract datos) $ randoms gen
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- 1NN Normal (pesos todo uno)
---------------------------------------------------------------------------------
pesosUno :: Algoritmo
pesosUno trainData = V.replicate (nCaract trainData) 1.0
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- RELIEF
---------------------------------------------------------------------------------
-- Algoritmo greedy RELIEF
relief :: Algoritmo
relief trainData =
  let pesosIni    = V.replicate (nCaract trainData) 0.0
      pesosNuevos = foldl (\acc x -> actualizaPesos x trainData acc) pesosIni trainData
      maxPeso     = V.maximum pesosNuevos
  in V.map (\x -> if x < 0 then 0 else x / maxPeso) pesosNuevos

-- Usa el punto y actualiza los pesos segun la distancia, coordenada a coordenada del mas cercano de su clase y distinto de su clase
actualizaPesos :: Dato -> Datos -> Pesos -> Pesos
actualizaPesos p trainData =
  let distIndice         = zipWith (\x y -> (dist1 p x, y)) trainData [0..(length trainData - 1)]
      distancias         = L.sortBy (\(x,_) (y,_) -> compare x y) distIndice
      iAmigo             = snd $ M.fromJust $ L.find (\(_,x) -> snd (trainData !! x) == snd p) $ tail distancias
      iEnemigo           = snd $ M.fromJust $ L.find (\(_,x) -> snd (trainData !! x) /= snd p) distancias
  in V.zipWith4 (\p' e a w -> p' `dist1c` e - p' `dist1c` a + w) (fst p) (fst $ trainData !! iEnemigo) (fst $ trainData !! iAmigo)

-- Distancia 1 entre dos puntos
dist1 :: Dato -> Dato -> Float
dist1 (p1,_) (p2,_) = V.sum $ V.zipWith dist1c p1 p2

-- Distancia 1 en una coordenada
dist1c :: Float -> Float -> Float
dist1c x y = abs (y - x)
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Búsqueda local
---------------------------------------------------------------------------------
-- Búsqueda local
busLoc :: StdGen -> Algoritmo
busLoc gen datos = getPesos $ evalState
  (hastaIteraciones (fParada datos) (mejorVecino datos) (pesosIniRand datos) 15000) gen

-- Condición de parada
fParada :: Datos -> Solucion -> Bool
fParada datos sol = getNVecinos sol >= 20 * (nCaract datos)

-- Crea una solución inicial con pesos aleatorios
pesosIniRand :: Datos -> Rand Solucion
pesosIniRand datos = do
  gen <- get
  let pesos = V.fromList $ take (nCaract datos) $ randomRs (0,1) gen
  put $ snd $ next gen
  return (crearSolucion datos pesos)

-- Obtengo los pesos vecinos a través de los pesos originales
obtenerPesosVecinos :: (Float, Float) -> [Int] -> Pesos -> Rand (Pesos, [Int])
obtenerPesosVecinos (mean, sD) indices pesos = do
  g <- get
  let (modif, g') = normal' (mean, sD) g
  let (ind, g'') = randomR (0, length indices - 1) g'
  put g''
  let i = indices L.!! ind
  let vNuevo = min 1 $ max 0 $ (pesos V.! i) + modif
  return (V.update pesos (V.fromList [(i, vNuevo)]), L.delete i indices)

-- Creo un nuevo vecino a partir de una solución
nuevoVecino :: Datos -> (Solucion, [Int]) -> Rand (Solucion, Solucion, [Int])
nuevoVecino datos (solActual, indices) = do
  let pesosOrig = getPesos solActual
  (pesosNuev, indNuev) <- obtenerPesosVecinos (0.0, 0.3) indices pesosOrig
  let solActualizada = aumentaVecino solActual
  return (solActualizada, crearSolucion datos pesosNuev, indNuev)

-- Bucle para seguir explorando el vecindario
hastaVecinos :: Datos -> (Datos -> (Solucion, [Int]) -> Rand (Solucion, Solucion, [Int])) -> (Solucion, [Int]) -> Rand (Solucion, [Int])
hastaVecinos datos f v = do
  (solActual, solVecina, indN) <- (f datos) v
  if getFit solActual < getFit solVecina then return (solVecina, [])
    else if (getNVecinos solActual >= 20 * (nCaract datos)) || (indN == []) then return (solActual, [])
      else hastaVecinos datos f (solActual, indN)

-- Nos da el mejor vecino de una solución (puede ser él mismo)
mejorVecino :: Datos -> Solucion -> Rand Solucion
mejorVecino datos solucionAct = do
  (solRes, _) <- hastaVecinos datos nuevoVecino (solucionAct, [0..(nCaract datos - 1)])
  return solRes

-- Bucle para seguir buscando soluciones
hastaIteraciones :: (Solucion -> Bool) -> (Solucion -> Rand Solucion) -> Rand Solucion -> Int -> Rand Solucion
hastaIteraciones p f m n = do
  x <- m
  if (p x) || (n == 0) then m else hastaIteraciones p f (f x) (n - 1)

-- Evaluación 1-NN con pesos
evaluarF :: Datos -> Pesos -> Float
evaluarF datos pesos =
  let pReduccion = (fromIntegral $ V.length $ V.filter (< 0.2) pesos) / (fromIntegral $ V.length pesos)
      pAcierto   = clas1nn datos datos pesos
  in fEvaluacion 0.5 pAcierto pReduccion
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------

-- Main
main :: IO ()
main = do
  args <- getArgs
  (nombreFichero, gen) <- (if (length args == 2) then do
    putStrLn "Seed fijada."
    let (nombreFichero:seed:_) = args
    return (nombreFichero, mkStdGen (read seed :: Int))
  else if (length args == 1) then do
      putStrLn "Seed aleatoria."
      let (nombreFichero:_) = args
      gen <- getStdGen
      return (nombreFichero, gen)
    else do
      putStrLn "Seed aleatoria."
      gen <- getStdGen
      return ("", gen))
  if nombreFichero == "" then do
    putStrLn $ "Ejecutando todos los archivos con seed " ++ show gen
    ejecutarTodos gen
  else do
    putStrLn $ "Ejecutando archivo " ++ show nombreFichero ++ " con seed " ++ show gen
    ejecutarPrograma gen nombreFichero
  putStrLn "Finalizando programa."

-- Ejecuta todos los dataset
ejecutarTodos :: StdGen -> IO ()
ejecutarTodos gen = do
  let nombresFicheros = ["./data/ionosphere.arff", "./data/datacolposcopy.arff", "./data/texture.arff"]
  _ <- mapM (ejecutarPrograma gen) nombresFicheros
  return ()

-- Ejecuta un dataset
ejecutarPrograma :: StdGen -> String -> IO ()
ejecutarPrograma gen nombreFichero = do
  fileExits <- doesFileExist nombreFichero
  if fileExits then do
    contenido <- readFile nombreFichero
    let dLeidos = crearDatos contenido
    let dNormalizados = normalizar dLeidos
    let dMezclados = shuffle' dNormalizados (length dNormalizados) gen
    let dClasificados = separarClases dMezclados
    let particiones = eleccionCruzada gen 5 dClasificados
    resultado <- ejecutarAlgoritmos particiones gen
    let ficheroResultado = takeBaseName nombreFichero ++ "_resultados.txt"
    writeFile ficheroResultado resultado
    putStrLn $ "Leído fichero " ++ nombreFichero ++ " sin errores."
  else putStrLn $ "Error: no se encuentra un archivo con el nombre " ++ nombreFichero

---------------------------------------------------------------------------------
