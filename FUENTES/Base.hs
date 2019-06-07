{-|
Module      : Base
Author      : Miguel Lentisco Ballesteros
Description : Implementa los tipos básicos y algunas funciones básicas
-}
module Base where

  {-# LANGUAGE StrictData, Strict #-}
  import qualified Data.Vector.Unboxed as U (Vector, length)
  import Control.Monad.State (State)
  import System.Random (StdGen)

  -- Una clase se puede representar como texto
  type Clase = String
  -- Un punto es un vector real
  type Punto = U.Vector Double
  -- Un dato es un vector de n caracteristicas reales (el punto) junto a su clase
  type Dato = (Punto, Clase)
  -- Una conjunto de datos
  type Datos = [Dato]
  -- Una partición esta compuesta de un conjunto de entramiento y uno de prueba
  type Particion = (Datos, Datos)
  -- Un conjunto de particiones
  type Particiones = [Particion]
  -- Unos pesos son un vector de n valores reales entre [0,1]
  type Pesos = U.Vector Double
  -- Un algoritmo es una función que lee unos datos (conjunto entrenamiento) y da una solución (pesos)
  type Algoritmo = Datos -> Pesos
  -- Definición de un evaulador de estados, con estado un generador y el nº de iteraciones realizadas
  -- En resumen: es para obtener valores aleatorios
  type Estado a = State (StdGen, Int) a

  -----------------------------------------------------------------------------
  -- P1
  -----------------------------------------------------------------------------
  -- Una solución con el valor fit guardado (y nº de vecinos creados para BL)
  data Solucion = Solucion {
    getPesos :: Pesos, -- La solución propiamente dicha (los pesos)
    getFit :: Double, -- El resultado de evaluar con la función objetivo los pesos de la solución
    getNVecinos :: Int -- Nº de vecinos en BL
  } deriving (Show)

  -- Dos soluciones son iguales si tienen los mismos pesos
  instance Eq Solucion where
    (Solucion p1 _ _) == (Solucion p2 _ _) = p1 == p2

  -- Para ordenar soluciones segun su fit
  instance Ord Solucion where
    (Solucion _ f1 _) <= (Solucion _ f2 _) = f1 <= f2
    (Solucion _ f1 _) > (Solucion _ f2 _) = f1 > f2
    compare s1 s2
      | s1 <= s2  = LT
      | otherwise = GT
    max s1 s2 = if getFit s1 > getFit s2 then s1 else s2
  -----------------------------------------------------------------------------
  -- P2
  -----------------------------------------------------------------------------
  -- Un gen es un valor real
  type Gen = Double
  -- Un cromosoma será una solución (pesos) con el valor de la función objetivo (de menor a mayor)
  type Cromosoma = Solucion
  -- Una población es una lista de cromosomas
  type Poblacion = [Cromosoma]
  -- Esquema creación población inicial: nº de poblacion y los datos para crearla
  type EsqInicial = Datos -> Estado Poblacion
  -- Esquema de seleccion: toma la población y devuelve los padres que van a cruzarse (sin ordenar)
  type EsqSeleccion = Poblacion -> Estado Poblacion
  -- El operador de cruce toma los 2 padres y devuelve los dos nuevos hijos creandolos con los datos
  type OpCruce = Datos -> Cromosoma -> Cromosoma -> Estado (Cromosoma, Cromosoma)
  -- Esquema de cruce: toma los padres que van a cruzarse, prob de cruce, los datos y devuelve la nueva población de hijos usando el op de cruce
  type EsqCruce = Double -> Datos -> OpCruce -> Poblacion -> Estado Poblacion
  -- El operador de mutación toma el hijo y la posición i-ésima donde tiene que mutar y devuelve un nuevo cromosoma
  type OpMutacion = Cromosoma -> [Int] -> Estado Cromosoma
  -- Esquema de mutación: toma la población y la prob de mutación y devuelve la población mutada con el operador de mutación
  type EsqMutacion = Double -> OpMutacion -> Poblacion -> Estado Poblacion
  -- El esquema de reemplazamiento toma la población actual, los hijos y devuelve la nueva población reemplazada
  type EsqReemp = Poblacion -> Poblacion -> Estado Poblacion
  -- El esquema de búsqueda local toma la población y la devuelve aplicando BL con los datos, segun un criterio
  type EsqBL = Datos -> Poblacion -> Estado Poblacion
  -----------------------------------------------------------------------------
  -- P3
  -----------------------------------------------------------------------------
  -- La temperatura es un Double
  type Temp = Double
  -----------------------------------------------------------------------------
  -- P4
  -----------------------------------------------------------------------------
  -- Esquema para mutar
  type EsqMutar = Poblacion -> Int -> Int -> Estado Double

  -----------------------------------------------------------------------------
  -- Funciones básicas
  -----------------------------------------------------------------------------

  -- Devuelve el nº de características que tiene un conjunto de datos
  nCaract :: Datos -> Int
  nCaract ((punto, _):_) = U.length punto

  nCaractGeneric :: (Num a) => Datos -> a
  nCaractGeneric = fromIntegral . nCaract

  getN :: Poblacion -> Int
  getN = U.length . getPesos . head

  getNGeneric :: (Num a) => Poblacion -> a
  getNGeneric = fromIntegral . getN

  -- Equivalencia de datos con clases
  mismaClase :: Dato -> Dato -> Bool
  mismaClase (_, c1) (_, c2) = c1 == c2

  -- Compara los datos por sus clases
  comparaClase :: Dato -> Dato -> Ordering
  comparaClase (_, c1) (_, c2) = compare c1 c2
