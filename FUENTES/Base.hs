{-|
Module      : Base
Author      : Miguel Lentisco Ballesteros
Description : Implementa los tipos básicos y algunas funciones básicas
-}
module Base where

  {-# LANGUAGE StrictData, Strict #-}
  import qualified Data.Vector.Unboxed as U (Vector, length)
  import Control.Monad.State (State, get, put)
  import System.Random (Random, StdGen, randomR, randomRs, next)
  import Data.Random.Normal (normal')


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

  -- Valor aleatorio uniformemente distribuido en [lo, hi]
  randR :: (Random a) => (a, a) -> Estado a
  randR (lo, hi) = do
    g <- getGen
    let (res, g') = randomR (lo, hi) g
    putGen g'
    return res

  -- Valor de distribución normal con media 0.0 y desviación estandar sD
  rNormal :: Float -> Estado Float
  rNormal sD = do
    g <- getGen
    let (res, g') = normal' (0.0, sD) g
    putGen g'
    return res

  randRs :: (Random a) => (a, a) -> Estado [a]
  randRs (lo, hi) = do
    g <- getGen
    putGen (snd $ next g)
    return $ randomRs (lo, hi) g

  -- Incremenenta en 1 el nº de iteraciones
  incIter :: Estado ()
  incIter = do
    nIter <- getIter
    putIter (nIter + 1)

  -- Devuelve el nº de iteraciones
  getIter :: Estado Int
  getIter = do
    (_, nIter) <- get
    return nIter

  -- Da el estado del generador
  getGen :: Estado StdGen
  getGen = do
    (gen, _) <- get
    return gen

  -- Establece el nº de iteraciones
  putIter :: Int -> Estado ()
  putIter nIter = do
    (gen, _) <- get
    put (gen, nIter)

  -- Establece el generador
  putGen :: StdGen -> Estado ()
  putGen gen = do
    (_, nIter) <- get
    put (gen, nIter)

  -- Devuelve el nº de características que tiene un conjunto de datos
  nCaract :: Datos -> Int
  nCaract ((punto, _):_) = U.length punto

  -- Equivalencia de datos con clases
  mismaClase :: Dato -> Dato -> Bool
  mismaClase (_, c1) (_, c2) = c1 == c2

  -- Compara los datos por sus clases
  comparaClase :: Dato -> Dato -> Ordering
  comparaClase (_, c1) (_, c2) = compare c1 c2
