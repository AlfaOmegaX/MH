{-|
Module      : P2
Author      : Miguel Lentisco Ballesteros
Description : P2 ~ Algoritmos genéticos y meméticos
-}
module P2 where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import Utils
  import qualified Data.Set as S (deleteMin, findMax, splitAt, insert, union, null, filter, elemAt, fromList, unions, empty, deleteAt)
  import qualified Data.Vector.Unboxed as U (Vector, imap, fromList, foldl, zipWithM, zipWith, empty, length)
  import Data.List (genericLength)
  import Control.Monad (zipWithM, replicateM)
  import Control.Monad.Extra (iterateM)
  import System.Random (StdGen)
  import Control.Monad.State (evalState)
  import Debug.Trace

  algoritmosP2 :: StdGen -> [(String, Algoritmo)]
  algoritmosP2 gen = [("AGG-BLX", aggBlx gen), ("AGG-CA", aggCa gen), ("AGE-BLX", ageBlx gen), ("AGE-CA", ageCa gen)]

  -- Genético generacional con cruce BLX
  aggBlx :: StdGen -> Algoritmo
  aggBlx = algGenetico 0.001 30 (blxAlpha 0.3) 0.7 15 reempGeneracional

  -- Genético generacional con cruce aritmético
  aggCa :: StdGen -> Algoritmo
  aggCa =  algGenetico 0.001 30 (cruceAritmetico 0.5) 0.7 15 reempGeneracional

  -- Genético estacionario con cruce BLX
  ageBlx :: StdGen -> Algoritmo
  ageBlx = algGenetico 0.001 30 (blxAlpha 0.3) 1.0 1 reempEstacionario

  -- Genético estacionario con cruce aritmético
  ageCa :: StdGen -> Algoritmo
  ageCa =  algGenetico 0.001 30 (cruceAritmetico 0.5) 1.0 1 reempEstacionario

  -- Esquema general de un algoritmo genético
  algGenetico :: Float -> Int -> OpCruce -> Float -> Int -> EsqReemp -> StdGen -> Algoritmo
  algGenetico pMut nPob opCruce pCruce nParejas esqReemp gen datos = getPesos $ S.findMax $ evalState (untilM (maxIteraciones 15000) generarPob (crearPobIni nPob datos)) (gen, 0)
    where generarPob pob = (seleccion nParejas pob) >>= (cruce (round (pCruce * fromIntegral nParejas)) datos opCruce) >>= mutacion (round (pMut * (fromIntegral nPob) * (fromIntegral $ nCaract datos))) (mutNormal 0.3 datos) >>= (esqReemp pob)

  -- Crea los cromosomas iniciales (aleatorios)
  crearCromIni :: Datos -> Estado Cromosoma
  crearCromIni datos =
    do
      nRandoms <- randRs (0.0, 1.0)
      let pesos = U.fromList $ take (nCaract datos) nRandoms
      crearCromosoma datos pesos

  -- Esquema de creación población iniciaL: crea nPob cromosomas
  crearPobIni :: EsqInicial
  crearPobIni nPob datos =
    do
      cromosomas <- replicateM nPob (crearCromIni datos)
      return $ S.fromList cromosomas

  -- Esquema de selección: Tomamos la población y seleccionamos el nº de padres (se toman tantas parejas como nParejas)
  seleccion :: Int -> EsqSeleccion
  seleccion nParejas pob = replicateM (nParejas * 2) (torneoN 2 pob)

  -- Esquema de cruce: tomamos la población y juntamos las nCruces parejas, uniendo el primero con el N / 2 y así
  cruce :: Int -> Datos -> EsqCruce
  cruce nCruces datos opCruce padres =
    do
      let (padres1, padres2) = splitAt (round $ genericLength padres / (2 :: Double)) padres
      let (cruce1, noCruce1) = splitAt nCruces padres1
      let (cruce2, noCruce2) = splitAt nCruces padres2
      let x = traceId "hola"
      hijosP <- zipWithM opCruce cruce1 cruce2
      hijos <- mapM (convertirHijos datos) hijosP
      let nPob = foldl (\acc (h1, h2) -> S.insert h2 $ S.insert h1 acc) S.empty hijos
      return $ S.unions [nPob, S.fromList noCruce1, S.fromList noCruce2]

-- Transforma el par de pesos en par de cromosomas
  convertirHijos :: Datos -> (Pesos, Pesos) -> Estado (Cromosoma, Cromosoma)
  convertirHijos datos (w1, w2) =
    do
      h1 <- crearCromosoma datos w1
      h2 <- crearCromosoma datos w2
      return (h1, h2)

  -- Esquema de mutación: tomamos la población y mutamos aleatoriamente tantas veces como nMut y con el op mutación
  mutacion :: Int -> EsqMutacion
  mutacion nMut opMut pob =
    do
      res <- iterateM (mutarCromosoma opMut) pob
      return (res !! nMut)

  -- Sacamos un cromosoma aleatorio y un indice aleatorio de su gen a mutar
  mutarCromosoma :: OpMutacion -> Poblacion -> Estado Poblacion
  mutarCromosoma opMut pob =
    do
      iCro <- randR (0, length pob - 1)
      let cromosoma = S.elemAt iCro pob
      iGen <- randR (0, U.length (getPesos cromosoma) - 1)
      cNuevo <- opMut cromosoma iGen
      return $ S.insert cNuevo $ S.deleteAt iCro pob

  -- Se escogen n individuos aleatoriamente de la población y se devuelve el que tenga mejor
  torneoN :: Int -> Poblacion -> Estado Cromosoma
  torneoN n pob =
    do
      indRand <- randRs (0, length pob - 1)
      let inds = take n $ indRand
      return $ foldl (\acc i -> max acc (S.elemAt i pob)) (Solucion U.empty 0.0 0) inds

  -- Esquema de reemplazamiento: los hijos reemplazan la población y si el mejor
  -- de la población anterior no está, se reemplaza por el peor hijo
  reempGeneracional :: EsqReemp
  reempGeneracional pActual hijos =
    if S.null $ S.filter (\x -> getPesos x == getPesos mejorP) hijos
      then return hijos
      else return $ S.insert mejorP (S.deleteMin hijos)
    where mejorP = S.findMax pActual

  -- Esquema de reemplazamiento: tipo estacionario - metemos a los dos hijos y se eliminan los 2 peores
  reempEstacionario :: EsqReemp
  reempEstacionario pActual hijos = return pNueva
    where (_, pNueva) = S.splitAt 2 $ S.union pActual hijos

  -- Operador de mutación: Mutamos el hijo en la posición gen-ésima con desviación estandar sD
  mutNormal :: Float -> Datos -> OpMutacion
  mutNormal sD datos hijo gen =
    do
      z <- rNormal sD
      let pesos = U.imap (\i x -> if i == gen then min 1.0 (max 0.0 (x + z)) else x) (getPesos hijo)
      crearCromosoma datos pesos

  -- Operador de cruce: cruce aritmético (combinación lineal)
  cruceAritmetico :: Float -> OpCruce
  cruceAritmetico alpha p1 p2 = do
    let cLineal i1 i2 = (alpha * i1 + (1 - alpha) * i2, (1 - alpha) * i1 + alpha * i2)
    let (w1, w2) = separarGenes $ U.zipWith cLineal (getPesos p1) (getPesos p2)
    return (w1, w2)

  -- Operador de cruce: blx-alpha
  blxAlpha :: Float -> OpCruce
  blxAlpha alpha p1 p2 =
    do
      res <- U.zipWithM (cruzarGenes alpha) (getPesos p1) (getPesos p2)
      let (w1, w2) = separarGenes res
      return (w1, w2)

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
