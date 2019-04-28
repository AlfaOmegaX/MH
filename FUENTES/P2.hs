{-|
Module      : P2
Author      : Miguel Lentisco Ballesteros
Description : P2 ~ Algoritmos genéticos y meméticos
-}
module P2 where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import Utils
  import qualified Data.Vector.Unboxed as U (Vector, imap, fromList, foldl, zipWithM, zipWith, empty, length)
  import Data.List (genericLength, sort, delete, maximum, minimum)
  import Control.Monad (zipWithM, replicateM)
  import System.Random (StdGen)
  import Control.Monad.State (evalState)

  algoritmosP2 :: StdGen -> [(String, Algoritmo)]
  --algoritmosP2 gen = [("AGG-BLX", aggBlx gen),("AGG-CA", aggCa gen), ("AGE-BLX", ageBlx gen), ("AGE-CA", ageCa gen)]
  algoritmosP2 gen = [("AGE-BLX", ageBlx gen)]
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
  algGenetico pMut nPob opCruce pCruce nParejas esqReemp gen datos = getPesos $ maximum $ evalState (hastaQueM (\_ -> maxIteraciones 15000) generarPob (crearPobIni nPob datos)) (gen, 0)
    where
      generarPob pob =
        do
          padres <- seleccion nParejas pob
          hijos <- cruce pCruce datos opCruce padres
          hMut <- mutacion pMut (mutNormal 0.3 datos) hijos
          esqReemp pob hMut

  -- Crea los cromosomas iniciales (aleatorios)
  crearCromIni :: Datos -> Estado Cromosoma
  crearCromIni datos =
    do
      nRandoms <- randRs (0.0, 1.0)
      let pesos = U.fromList $ take (nCaract datos) nRandoms
      crearCromosoma datos pesos

  -- Esquema de creación población iniciaL: crea nPob cromosomas
  crearPobIni :: EsqInicial
  crearPobIni nPob datos = replicateM nPob (crearCromIni datos)

  -- Esquema de selección: Tomamos la población y seleccionamos el nº de padres (se toman tantas parejas como nParejas)
  seleccion :: Int -> EsqSeleccion
  seleccion nParejas pob = replicateM (nParejas * 2) (torneoN 2 pob)

  -- Se escogen n individuos aleatoriamente de la población y se devuelve el que tenga mejor
  torneoN :: Int -> Poblacion -> Estado Cromosoma
  torneoN n pob =
    do
      indRand <- randRs (0, length pob - 1)
      let inds = take n indRand
      return $ foldl (\acc i -> max acc (pob !! i)) (Solucion U.empty 0.0 0) inds

  -- Esquema de cruce: tomamos la población y juntamos las nCruces parejas, uniendo el primero con el N / 2 y así
  cruce :: Float -> Datos -> EsqCruce
  cruce pCruce datos opCruce padres =
    do
      let nCruces = round $ pCruce * genericLength padres / (2 :: Float)
      let (cruces, noCruce) = splitAt (nCruces * 2) padres
      let (padres1, padres2) = splitAt (nCruces) cruces
      hijosP <- zipWithM opCruce padres1 padres2
      hijos <- mapM (convertirHijos datos) hijosP
      let nPob = foldl (\acc (h1, h2) -> h1:h2:acc) [] hijos
      return $ nPob ++ noCruce

-- Transforma el par de pesos en par de cromosomas
  convertirHijos :: Datos -> (Pesos, Pesos) -> Estado (Cromosoma, Cromosoma)
  convertirHijos datos (w1, w2) =
    do
      h1 <- crearCromosoma datos w1
      h2 <- crearCromosoma datos w2
      return (h1, h2)

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
    
  -- Función axuliar: transforma un vector de (Gen, Gen) en (Pesos, Pesos)
  separarGenes :: U.Vector (Gen, Gen) -> (Pesos, Pesos)
  separarGenes genes = (U.fromList h1, U.fromList h2)
    where (h1, h2) = U.foldl (\(acc1, acc2) (g1, g2) -> (acc1 ++ [g1], acc2 ++ [g2])) ([], []) genes

  -- Esquema de mutación: tomamos la población y mutamos aleatoriamente tantas veces como nMut y con el op mutación
  mutacion :: Float -> EsqMutacion
  mutacion pMut opMut pob = repiteNM (round (genericLength pob * n * pMut)) (mutarCromosoma opMut) pob
    where n = fromIntegral $ U.length $ getPesos $ head pob

  -- Sacamos un cromosoma aleatorio y un indice aleatorio de su gen a mutar
  mutarCromosoma :: OpMutacion -> Poblacion -> Estado Poblacion
  mutarCromosoma opMut pob =
    do
      iCro <- randR (0, length pob - 1)
      let cromosoma = pob !! iCro
      iGen <- randR (0, U.length (getPesos cromosoma) - 1)
      cNuevo <- opMut cromosoma iGen
      return $ (delete cromosoma pob) ++ [cNuevo]

  -- Operador de mutación: Mutamos el hijo en la posición gen-ésima con desviación estandar sD
  mutNormal :: Float -> Datos -> OpMutacion
  mutNormal sD datos hijo gen =
    do
      z <- rNormal sD
      let pesos = U.imap (\i x -> if i == gen then min 1.0 (max 0.0 (x + z)) else x) (getPesos hijo)
      crearCromosoma datos pesos

  -- Esquema de reemplazamiento: los hijos reemplazan la población y si el mejor
  -- de la población anterior no está, se reemplaza por el peor hijo
  reempGeneracional :: EsqReemp
  reempGeneracional pActual hijos =
    if elem mejorP hijos
      then return hijos
      else return $ (delete (minimum hijos) hijos) ++ [mejorP]
    where mejorP = maximum pActual

  -- Esquema de reemplazamiento: tipo estacionario - metemos a los dos hijos y se eliminan los 2 peores
  reempEstacionario :: EsqReemp
  reempEstacionario pActual hijos = return $ drop 2 $ sort $ pActual ++ hijos
