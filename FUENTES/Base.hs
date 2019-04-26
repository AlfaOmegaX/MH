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
  import Data.Set(Set)


  -- Una clase se puede representar como texto
  type Clase = String
  -- Un punto es un vector real
  type Punto = U.Vector Float
  -- Un dato es un vector de n caracteristicas reales (el punto) junto a su clase
  type Dato = (Punto, Clase)
  -- Una conjunto de datos
  type Datos = [Dato]
  -- Una partición esta compuesta de un conjunto de entramiento y uno de prueba
  type Particion = (Datos, Datos)
  -- Un conjunto de particiones
  type Particiones = [Particion]
  -- Unos pesos son un vector de n valores reales entre [0,1]
  type Pesos = U.Vector Float
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
    getFit :: Float, -- El resultado de evaluar con la función objetivo los pesos de la solución
    getNVecinos :: Int -- Nº de vecinos en BL
  }

  -- Para evitar que se eliminen soluciones duplicadas
  instance Eq Solucion where
    _ == _ = False

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
  type Gen = Float
  -- Un cromosoma será una solución (pesos) con el valor de la función objetivo (de menor a mayor)
  type Cromosoma = Solucion
  -- Una población es un conjunto de cromosomas
  type Poblacion = Set Cromosoma
  -- El operador de cruce toma los 2 padres y devuelve 2 pesos para convertir en hijos
  type OpCruce = Cromosoma -> Cromosoma -> Estado (Cromosoma, Cromosoma)
  -- El operador de mutación toma el hijo y la posición i-ésima donde tiene que mutar y devuelve un nuevo cromosoma
  type OpMutacion = Cromosoma -> Int -> Estado Cromosoma
  -- El esquema de reemplazamiento toma la población actual, los hijos y devuelve la nueva población reemplazada
  type EsqReemplazamiento = Poblacion -> Poblacion -> Poblacion
  -- Esquema de seleccion: toma la población y devuelve los padres que van a cruzarse (sin ordenar)
  type EsqSeleccion = Poblacion -> Estado [Cromosoma]
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Funciones básicas
  -----------------------------------------------------------------------------

  -- Devuelve el nº de características que tiene un conjunto de datos
  nCaract :: Datos -> Int
  nCaract ((punto, _):_) = U.length punto

  -- Equivalencia de datos con clases
  mismaClase :: Dato -> Dato -> Bool
  mismaClase (_, c1) (_, c2) = c1 == c2

  -- Compara los datos por sus clases
  comparaClase :: Dato -> Dato -> Ordering
  comparaClase (_, c1) (_, c2) = compare c1 c2
