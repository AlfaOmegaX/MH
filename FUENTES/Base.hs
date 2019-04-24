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

  -- Devuelve el nº de características que tiene un conjunto de datos
  nCaract :: Datos -> Int
  nCaract ((punto, _):_) = U.length punto

  -- Equivalencia de datos con clases
  mismaClase :: Dato -> Dato -> Bool
  mismaClase (_, c1) (_, c2) = c1 == c2

  -- Compara los datos por sus clases
  comparaClase :: Dato -> Dato -> Ordering
  comparaClase (_, c1) (_, c2) = compare c1 c2
