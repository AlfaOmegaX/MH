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
  --import qualified Data.HashMap.Strict as S (HashMap, lookup, insert, empty)

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
    in fEvaluacion 0.5 (pAcierto * 100) (pReduccion * 100)
