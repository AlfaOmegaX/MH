{-|
Module      : Utils
Author      : Miguel Lentisco Ballesteros
Description : Implementa tipos y funciones útiles para todas las prácticas
-}
module Utils where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import KNN
  import Control.Monad.State (get, put)
  import System.Random (Random, StdGen, randomR, randomRs, next)
  import Data.Random.Normal (normal')
  import Data.Vector.Unboxed as U (fromList)

  ---------------------------------------------------------------------------------
  -- Bucles de control
  ---------------------------------------------------------------------------------
  -- Hasta que se cumpla el predicado p aplica f con valor inicial m
  hastaQueM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
  hastaQueM p f = bucleM
    where
      bucleM m =
        do
          x <- m
          y <- p x
          if y then m else bucleM (f x)

  repiteNM :: Monad m => Int -> (a -> m a) -> a -> m a
  repiteNM 0 _ x = return x
  repiteNM n f x = bucleM n (return x)
    where
      bucleM i m =
        do
          v <- m
          if i > 0 then bucleM (i - 1) (f v) else m


  maxIteraciones :: Int -> Estado Bool
  maxIteraciones maxIter =
    do
      n <- getIter
      return $ n >= maxIter

  maxVecinos :: Int -> Solucion -> Bool
  maxVecinos maxVec sol = getNVecinos sol >= maxVec

  condParada :: Int -> (Solucion -> Bool) -> Solucion -> Estado Bool
  condParada maxIter condicionExtra sol =
    do
      c <- maxIteraciones maxIter
      return $ c || condicionExtra sol

  sinMejora :: Int -> Solucion -> Bool
  sinMejora nVecExi _ = nVecExi == 0

  ---------------------------------------------------------------------------------
  -- Funciones para soluciones
  ---------------------------------------------------------------------------------
  -- Crea un objeto Solucion a partir de unos datos y unos pesos
  crearSolucion :: Datos -> Pesos -> Estado Solucion
  crearSolucion datos pesos =
    do
      incIter
      return $ Solucion pesos (evaluarF datos pesos) 0

  -- Aumenta en un uno el nº de vecinos de una solución
  aumentaVecino :: Solucion -> Solucion
  aumentaVecino sol = Solucion (getPesos sol) (getFit sol) (getNVecinos sol + 1)

  -- Crear cromosoma
  crearCromosoma :: Datos -> Pesos -> Estado Cromosoma
  crearCromosoma = crearSolucion

  -- Crea una solución inicial con pesos aleatorios
  pesosIniRand :: Datos -> Estado Solucion
  pesosIniRand datos = do
    listaRands <- randRs (0.0, 1.0)
    let pesos = U.fromList $ take (nCaract datos) listaRands
    crearSolucion datos pesos
  ---------------------------------------------------------------------------------
  -- Funciones para Estado
  ---------------------------------------------------------------------------------

  -- Valor aleatorio uniformemente distribuido en [lo, hi]
  randR :: (Random a) => (a, a) -> Estado a
  randR (lo, hi) = do
    g <- getGen
    let (res, g') = randomR (lo, hi) g
    putGen g'
    return res

  -- Valor de distribución normal con media 0.0 y desviación estandar sD
  rNormal :: Double -> Estado Double
  rNormal sD = do
    g <- getGen
    let (res, g') = normal' (0.0, sD) g
    putGen g'
    return res

  -- Lista infinita de valores uniformemente distribuidos en [lo, hi]
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

  -- Restringe el valor al inervalo [0,1]
  restringe :: Double -> Double
  restringe = min 1.0 . max 0.0
