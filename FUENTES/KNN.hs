{-|
Module      : KNN
Author      : Miguel Lentisco Ballesteros
Description : Implementa K-nearest neighbours
-}
module KNN where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import qualified Data.Vector.Unboxed as U (zipWith3, sum, filter, length, map)
  import Data.List (delete, genericLength)
  import qualified Data.HashMap.Strict as S (HashMap, lookup, insert, empty)
  --import Debug.Trace

  ---------------------------------------------------------------------------------
  -- 1-NN
  ---------------------------------------------------------------------------------
  -- pre: los pesos ya están reducidos
  -- Clasificador 1-nn con pesos, devuelve el porcentaje de acierto
  clas1nn :: Datos -> Datos -> Pesos -> Double
  clas1nn lTrain lTest pesos =
    let maxValue p = fromIntegral $ length p + 1
        minDist p = foldl (min2P p pesos) (maxValue p, "") (delete p lTrain)
        aciertos  = fmap (\p -> snd p == snd (minDist p)) lTest
    in genericLength (filter (== True) aciertos) / genericLength aciertos

  clas1nnTrain :: Datos -> Pesos -> Double
  clas1nnTrain datos pesos =
    let (aciertos, _, _) = foldl (aciertaPunto datos pesos) ([], 0, S.empty) datos
    in genericLength (filter (== True) aciertos) / genericLength aciertos

  aciertaPunto :: Datos -> Pesos -> ([Bool], Int, S.HashMap Int Double) -> Dato -> ([Bool], Int, S.HashMap Int Double)
  aciertaPunto datos pesos (acc, ind, dist) x =
    let maxValue p  = fromIntegral $ length p + 1
        n           = length datos
        minDist p dists i = foldl (min2PHashMap n pesos p i) (maxValue p, "", 0, dists) (delete p datos)
        (_, c, _, distsN) = minDist x dist ind
    in ((snd x == c):acc, ind + 1, distsN)

  min2PHashMap :: Int -> Pesos -> Dato -> Int -> (Double, Clase, Int, S.HashMap Int Double) -> Dato -> (Double, Clase, Int, S.HashMap Int Double)
  min2PHashMap n pesos p i (acc, c, j, dists) x =
    let (distsN, d) = accedeDist n pesos p x dists i j
    in if d < acc then (d, snd x, j + 1, distsN) else (acc, c, j + 1, distsN)

  accedeDist :: Int -> Pesos -> Dato -> Dato -> S.HashMap Int Double -> Int -> Int -> (S.HashMap Int Double, Double)
  accedeDist n pesos p1 p2 dists i j =
    let indice = if i > j then tIndice n j (i - 1) else tIndice n i j
    in case S.lookup indice dists of
        Nothing -> insertaDistancia pesos p1 p2 dists indice
        Just d -> (dists, d)

  -- Inserta en HashMap la distancia entre 2 puntos
  insertaDistancia :: Pesos -> Dato -> Dato -> S.HashMap Int Double -> Int -> (S.HashMap Int Double, Double)
  insertaDistancia pesos p1 p2 dists ind =
    let d = dist2P p1 p2 pesos
    in (S.insert ind d dists, d)

  -- Transforma el i j en un indice de HashMap
  tIndice :: Int -> Int -> Int -> Int
  tIndice n i j = ((i * (2 * n - i - 3)) `quot` 2) + j

  -- Distancia mínima de las dos
  min2P :: Dato -> Pesos -> (Double, Clase) -> Dato -> (Double, Clase)
  min2P p pesos acc x = if distP < fst acc then (distP, snd x) else acc
    where distP = dist2P x p pesos

  -- Distancia euclidea considerando pesos
  dist2P :: Dato -> Dato -> Pesos -> Double
  dist2P (p1,_) (p2,_) pesos = U.sum $ U.zipWith3 (\x y z -> z * rest y x * rest y x) p1 p2 pesos
    where rest y x = y - x

  -- Reduce los pesos (pone a 0 los pesos que valgan menos de 0.2)
  reducePesos :: Pesos -> Pesos
  reducePesos = U.map (\x -> if x < 0.2 then 0.0 else x)
  ---------------------------------------------------------------------------------
  -- Evaluaciones de función objetivo
  ---------------------------------------------------------------------------------
  -- Función objetivo general
  fEvaluacion :: Double -> Double -> Double -> Double
  fEvaluacion alpha tAcier tRed = alpha * tAcier + (1 - alpha) * tRed

  -- Evaluación 1-NN con pesos ~ para algoritmos
  evaluarF :: Datos -> Pesos -> Double
  evaluarF datos pesos =
    let pReduccion = fromIntegral (U.length (U.filter (< 0.2) pesos)) / fromIntegral (U.length pesos)
        pAcierto   = clas1nn datos datos $ reducePesos pesos
    in fEvaluacion 0.5 pAcierto pReduccion
