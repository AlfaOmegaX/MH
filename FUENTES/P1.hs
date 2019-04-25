{-|
Module      : P1
Author      : Miguel Lentisco Ballesteros
Description : P1 ~ Algoritmos RELIEF, BL, Pesos Uno, Pesos Aleatorios
-}

module P1 where

{-# LANGUAGE StrictData, Strict #-}
import Base
import KNN
import qualified Data.Vector.Unboxed as U (fromList, replicate, map, maximum, minimum, zipWith, zipWith4, sum, update, (!))
import Data.List ((\\), sortBy, find, delete, (!!))
import Data.Maybe (fromJust)
import System.Random (StdGen, randoms)
import Control.Monad.State (evalState)
--import Debug.Trace

-- Lista de algoritmos
algoritmosP1 :: StdGen -> [(String, Algoritmo)]
algoritmosP1 gen = [("Pesos aleatorios", pesosRand gen), ("Pesos Uno", pesosUno), ("Relief", relief),  ("Busqueda local", busLoc gen)]

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

---------------------------------------------------------------------------------
-- Pesos aleatorios
---------------------------------------------------------------------------------
pesosRand :: StdGen -> Algoritmo
pesosRand gen datos = U.fromList $ take (nCaract datos) $ randoms gen
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- 1NN Normal (pesos todo uno)
---------------------------------------------------------------------------------
pesosUno :: Algoritmo
pesosUno trainData = U.replicate (nCaract trainData) 1.0
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- RELIEF
---------------------------------------------------------------------------------
-- Algoritmo greedy RELIEF
relief :: Algoritmo
relief trainData =
  let pesosIni       = U.replicate (nCaract trainData) 0.0
      pesosNuevos    = foldl (\acc x -> actualizaPesos x trainData acc) pesosIni trainData
      pesosPositivos = U.map (\x -> if x < 0.0 then 0.0 else x) pesosNuevos
      (pMax, pMin)   = (U.maximum pesosPositivos, U.minimum pesosPositivos)
  in U.map (\x -> if pMax == pMin then pMax else (x - pMin) / (pMax - pMin)) pesosNuevos

-- Usa el punto y actualiza los pesos segun la distancia, coordenada a coordenada del mas cercano de su clase y distinto de su clase
actualizaPesos :: Dato -> Datos -> Pesos -> Pesos
actualizaPesos p datos =
  let trainData          = datos \\ [p]
      distIndice         = zipWith (\x y -> (dist1 p x, y)) trainData [0..(length trainData - 1)]
      distancias         = sortBy (\(x,_) (y,_) -> compare x y) distIndice
      iAmigo             = snd $ fromJust $ find (\(_,x) -> snd (trainData !! x) == snd p) $ tail distancias
      iEnemigo           = snd $ fromJust $ find (\(_,x) -> snd (trainData !! x) /= snd p) distancias
  in U.zipWith4 (\p' e a w -> p' `dist1c` e - p' `dist1c` a + w) (fst p) (fst $ trainData !! iEnemigo) (fst $ trainData !! iAmigo)

-- Distancia 1 entre dos puntos
dist1 :: Dato -> Dato -> Float
dist1 (p1,_) (p2,_) = U.sum $ U.zipWith dist1c p1 p2

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
  (hastaIteraciones datos (mejorVecino datos) (pesosIniRand datos)) (gen, 0)

-- Bucle para seguir buscando soluciones
hastaIteraciones :: Datos -> (Solucion -> Estado Solucion) -> Estado Solucion -> Estado Solucion
hastaIteraciones datos f m = do
  nIter <- getIter
  x <- m
  if (getNVecinos x >= 20 * (nCaract datos)) || (nIter >= 15000) then m else hastaIteraciones datos f (f x)

-- Crea una solución inicial con pesos aleatorios
pesosIniRand :: Datos -> Estado Solucion
pesosIniRand datos = do
  listaRands <- randRs (0.0, 1.0)
  let pesos = U.fromList $ take (nCaract datos) listaRands
  incIter
  return (crearSolucion datos pesos)

-- Nos da el mejor vecino de una solución (puede ser él mismo)
mejorVecino :: Datos -> Solucion -> Estado Solucion
mejorVecino datos solucionAct = do
  (solRes, _) <- hastaVecinos datos nuevoVecino (solucionAct, [0..(nCaract datos - 1)])
  return solRes

-- Bucle para seguir explorando el vecindario
hastaVecinos :: Datos -> (Datos -> (Solucion, [Int]) -> Estado (Solucion, Solucion, [Int])) -> (Solucion, [Int]) -> Estado (Solucion, [Int])
hastaVecinos datos f v = do
  nIter <- getIter
  (solActual, solVecina, indN) <- (f datos) v
  if getFit solActual < getFit solVecina then return (solVecina, [])
    else if (getNVecinos solActual >= 20 * (nCaract datos)) || (indN == []) || (nIter >= 15000) then return (solActual, [])
      else hastaVecinos datos f (solActual, indN)

-- Creo un nuevo vecino a partir de una solución
nuevoVecino :: Datos -> (Solucion, [Int]) -> Estado (Solucion, Solucion, [Int])
nuevoVecino datos (solActual, indices) = do
  let pesosOrig = getPesos solActual
  (pesosNuev, indNuev) <- obtenerPesosVecinos 0.3 indices pesosOrig
  let solActualizada = aumentaVecino solActual
  incIter
  return (solActualizada, crearSolucion datos pesosNuev, indNuev)

-- Obtengo los pesos vecinos a través de los pesos originales
obtenerPesosVecinos :: Float -> [Int] -> Pesos -> Estado (Pesos, [Int])
obtenerPesosVecinos sD indices pesos = do
  modif <- rNormal sD
  ind <- randR (0, length indices - 1)
  let i = indices !! ind
  let vNuevo = min 1.0 $ max 0.0 $ (pesos U.! i) + modif
  return (U.update pesos (U.fromList [(i, vNuevo)]), delete i indices)
---------------------------------------------------------------------------------
