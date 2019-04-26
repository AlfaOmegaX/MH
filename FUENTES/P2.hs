{-|
Module      : P2
Author      : Miguel Lentisco Ballesteros
Description : P2 ~ Algoritmos genéticos y meméticos
-}
module P2 where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import Utils
  import qualified Data.Set as S (deleteMin, findMax, splitAt, insert, union, null, filter, elemAt, fromList, unions, empty)
  import qualified Data.Vector.Unboxed as U (Vector, imap, fromList, foldl, zipWithM, zipWith, empty)
  import Data.List (genericLength)
  import Control.Monad (zipWithM, replicateM)

  --algoritmosP2 :: StdGen -> [(String, Algoritmo)]
  --algoritmosP2 = []

  -- Esquema de cruce: tomamos la población y juntamos las nCruces parejas, uniendo el primero con el N / 2 y así
  cruce :: OpCruce -> Int -> [Cromosoma] -> Estado Poblacion
  cruce opCruce nCruces padres = do
    let (padres1, padres2) = splitAt (round $ genericLength padres / 2) padres
    let (cruce1, noCruce1) = splitAt nCruces padres1
    let (cruce2, noCruce2) = splitAt nCruces padres2
    hijos <- zipWithM opCruce cruce1 cruce2
    let nPob = foldl (\acc (h1, h2) -> S.insert h2 $ S.insert h1 acc) S.empty hijos
    return $ S.unions [nPob, S.fromList noCruce1, S.fromList noCruce2]

  -- Tomamos la población y seleccionamos el nº de padres (se toman tantas parejas como nParejas)
  seleccion :: Int -> EsqSeleccion
  seleccion nParejas pob = replicateM (nParejas * 2) (torneoN 2 pob)

  -- Se escogen n individuos aleatoriamente de la población y se devuelve el que tenga mejor
  torneoN :: Int -> Poblacion -> Estado Cromosoma
  torneoN n pob =
    do
      indRand <- randRs (0, length pob - 1)
      let inds = take n $ indRand
      return $ foldl (\acc i -> max acc (S.elemAt i pob)) (Solucion U.empty 0.0 0) inds

  -- Esquema de reemplazamiento: los hijos reemplazan la población y si el mejor
  -- de la población anterior no está, se reemplaza por el peor hijo
  reempGeneracional :: EsqReemplazamiento
  reempGeneracional pActual hijos =
    if S.null $ S.filter (\x -> getPesos x == getPesos mejorP) hijos
      then hijos
      else S.insert mejorP (S.deleteMin hijos)
    where mejorP = S.findMax pActual

  -- Esquema de reemplazamiento: tipo estacionario - metemos a los dos hijos y se eliminan los 2 peores
  reempEstacionario :: EsqReemplazamiento
  reempEstacionario pActual hijos = pNueva
    where (_, pNueva) = S.splitAt 2 $ S.union pActual hijos

  -- Operador de mutación: Mutamos el hijo en la posición gen-ésima con desviación estandar sD
  mutarCromosoma :: Float -> Datos -> OpMutacion
  mutarCromosoma sD datos hijo gen =
    do
      z <- rNormal sD
      let pesos = U.imap (\i x -> if i == gen then min 1.0 (max 0.0 (x + z)) else x) (getPesos hijo)
      crearCromosoma datos pesos

  -- Operador de cruce: cruce aritmético (combinación lineal)
  cruceAritmetico :: Float -> Datos -> OpCruce
  cruceAritmetico alpha dataS p1 p2 = do
    let cLineal i1 i2 = (alpha * i1 + (1 - alpha) * i2, (1 - alpha) * i1 + alpha * i2)
    let (w1, w2) = separarGenes $ U.zipWith cLineal (getPesos p1) (getPesos p2)
    c1 <- crearCromosoma dataS w1
    c2 <- crearCromosoma dataS w2
    return (c1, c2)

  -- Operador de cruce: blx-alpha
  blxAlpha :: Float -> Datos -> OpCruce
  blxAlpha alpha dataS p1 p2 =
    do
      res <- U.zipWithM (cruzarGenes alpha) (getPesos p1) (getPesos p2)
      let (w1, w2) = separarGenes res
      c1 <- crearCromosoma dataS w1
      c2 <- crearCromosoma dataS w2
      return (c1, c2)

  -- Aux
  separarGenes :: U.Vector (Gen, Gen) -> (Pesos, Pesos)
  separarGenes genes = (U.fromList h1, U.fromList h2)
    where (h1, h2) = U.foldl (\(acc1, acc2) (g1, g2) -> (acc1 ++ [g1], acc2 ++ [g2])) ([], []) genes

  -- Creamos los dos i-esimo gen para blx
  cruzarGenes :: Float -> Gen -> Gen -> Estado (Gen, Gen)
  cruzarGenes alpha i1 i2 =
    do
      let (cMax, cMin) = (max i1 i2, min i1 i2)
      let l = cMax - cMin
      let (a, b) = (cMin - l * alpha, cMax + l * alpha)
      listaRands <- randRs (a, b)
      let (g1:g2:_) = take 2 listaRands
      return (g1, g2)
