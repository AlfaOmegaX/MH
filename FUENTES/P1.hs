{-|
Module      : P1
Author      : Miguel Lentisco Ballesteros
Description : P1 ~ Algoritmos RELIEF, BL, Pesos Uno, Pesos Aleatorios
-}

module P1 where

{-# LANGUAGE StrictData, Strict #-}
import Base
import Utils
import qualified Data.Vector.Unboxed as U (replicate, map, maximum, minimum, zipWith, zipWith4, sum, imap)
import Data.List (sortBy, find, delete, (!!))
import Data.Maybe (fromJust)
import System.Random (StdGen)
import Control.Monad.State (evalState)

-- Lista de algoritmos
algoritmosP1 :: StdGen -> [(String, Algoritmo)]
algoritmosP1 gen = [("Pesos aleatorios", pesosRand gen), ("Pesos Uno", pesosUno), ("Relief", relief),  ("Busqueda local", busLoc gen)]

---------------------------------------------------------------------------------
-- Pesos aleatorios
---------------------------------------------------------------------------------
pesosRand :: StdGen -> Algoritmo
pesosRand gen datos = getPesos $ evalState (pesosIniRand datos) (gen, 0)
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
  let trainData          = delete p datos
      distIndice         = zipWith (\x y -> (dist1 p x, y)) trainData [0..(length trainData - 1)]
      distancias         = sortBy (\(x,_) (y,_) -> compare x y) distIndice
      iAmigo             = snd $ fromJust $ find (\(_,x) -> snd (trainData !! x) == snd p) $ tail distancias
      iEnemigo           = snd $ fromJust $ find (\(_,x) -> snd (trainData !! x) /= snd p) distancias
  in U.zipWith4 (\p' e a w -> p' `dist1c` e - p' `dist1c` a + w) (fst p) (fst $ trainData !! iEnemigo) (fst $ trainData !! iAmigo)

-- Distancia 1 entre dos puntos
dist1 :: Dato -> Dato -> Double
dist1 (p1,_) (p2,_) = U.sum $ U.zipWith dist1c p1 p2

-- Distancia 1 en una coordenada
dist1c :: Double -> Double -> Double
dist1c x y = abs (y - x)
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Búsqueda local
---------------------------------------------------------------------------------
-- Búsqueda local
busLoc :: StdGen -> Algoritmo
busLoc gen datos = getPesos $ fst $ evalState
  (hastaQueM (condParada 15000 (maxVecinos $ 20 * nCaract datos) . fst) (crearVecino datos) (pesosIniBL datos)) (gen, 0)

-- Crea una solución inicial con pesos aleatorios
pesosIniBL :: Datos -> Estado (Solucion, [Int])
pesosIniBL datos = do
  solRand <- pesosIniRand datos
  return (solRand, [0..(nCaract datos - 1)])

-- Creo un nuevo vecino a partir de una solución
crearVecino :: Datos -> (Solucion, [Int]) -> Estado (Solucion, [Int])
crearVecino datos (sol, indices) = do
  (solNueva, indNuev) <- obtenerVecino 0.3 datos indices sol
  let solAct = aumentaVecino sol
  let indNuev' = if solNueva > solAct || null indNuev then [0..(nCaract datos - 1)] else indNuev
  return (max solNueva solAct, indNuev')

-- Obtengo una nueva solución del vecindario de la solución actual
obtenerVecino :: Double -> Datos -> [Int] -> Solucion -> Estado (Solucion, [Int])
obtenerVecino sD datos indices sol = do
  inds <- randR (0, length indices - 1)
  let ind = indices !! inds
  z <- rNormal sD
  let pesosN = U.imap (\i x -> if i == ind then restringe $ x + z else x) $ getPesos sol
  nuevaSol <- crearSolucion datos pesosN
  return (nuevaSol, delete ind indices)
---------------------------------------------------------------------------------
