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

  ---------------------------------------------------------------------------------
  -- 1-NN
  ---------------------------------------------------------------------------------
  -- pre: los pesos ya están reducidos
  -- Clasificador 1-nn con pesos, devuelve el porcentaje de acierto
  clas1nn :: Datos -> Datos -> Pesos -> Float
  clas1nn lTrain lTest pesos =
    let minDist p = foldl (min2P p pesos) (fromIntegral $ length p + 1, "") (delete p lTrain)
        aciertos  = fmap (\p -> snd p == (snd $ minDist p)) lTest
    in (genericLength $ filter (== True) aciertos) / genericLength aciertos

  -- Distancia mínima de las dos
  min2P :: Dato -> Pesos -> (Float, Clase) -> Dato -> (Float, Clase)
  min2P p pesos acc x = if distP < fst acc then (distP, snd x) else acc
    where distP = dist2P x p pesos

  -- Distancia euclidea considerando pesos
  dist2P :: Dato -> Dato -> Pesos -> Float
  dist2P (p1,_) (p2,_) pesos = U.sum $ U.zipWith3 (\x y z -> z * rest y x * rest y x) p1 p2 pesos
    where rest y x = (y - x)

  -- Reduce los pesos (pone a 0 los pesos que valgan menos de 0.2)
  reducePesos :: Pesos -> Pesos
  reducePesos = U.map (\x -> if x < 0.2 then 0.0 else x)
  ---------------------------------------------------------------------------------
  -- Evaluaciones de función objetivo
  ---------------------------------------------------------------------------------
  -- Función objetivo general
  fEvaluacion :: Float -> Float -> Float -> Float
  fEvaluacion alpha tAcier tRed = alpha * tAcier + (1 - alpha) * tRed

  -- Evaluación 1-NN con pesos ~ para algoritmos
  evaluarF :: Datos -> Pesos -> Float
  evaluarF datos pesos =
    let pReduccion = (fromIntegral $ U.length $ U.filter (< 0.2) pesos) / (fromIntegral $ U.length pesos)
        pAcierto   = clas1nn datos datos $ reducePesos pesos
    in fEvaluacion 0.5 pAcierto pReduccion
