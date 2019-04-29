{-|
Module      : P2
Author      : Miguel Lentisco Ballesteros
Description : P2 ~ Algoritmos genéticos y meméticos
-}
module P2 where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import Utils
  import qualified Data.Vector.Unboxed as U (Vector, imapM, fromList, foldl, zipWithM, zipWith, empty, length)
  import Data.List (genericLength, sort, delete, maximum, minimum, group)
  import Control.Monad (zipWithM, replicateM, foldM)
  import Control.Monad.Extra (ifM)
  import System.Random (StdGen)
  import Control.Monad.State (evalState)

  algoritmosP2 :: StdGen -> [(String, Algoritmo)]
  --algoritmosP2 gen = [("AGG-BLX", aggBlx gen),("AGG-CA", aggCa gen), ("AGE-BLX", ageBlx gen), ("AGE-CA", ageCa gen)]
  algoritmosP2 gen = [("AGE-BLX", ageBlx gen), ("AGE-CA", ageCa gen)]
  -- Genético generacional con cruce BLX
  aggBlx :: StdGen -> Algoritmo
  aggBlx = algGenetico 30 (blxAlpha 0.3) 0.7 15 (mutGeneracional 0.001) reempGeneracional

  -- Genético generacional con cruce aritmético
  aggCa :: StdGen -> Algoritmo
  aggCa =  algGenetico 30 (cruceAritmetico 0.5) 0.7 15 (mutGeneracional 0.001) reempGeneracional

  -- Genético estacionario con cruce BLX
  ageBlx :: StdGen -> Algoritmo
  ageBlx = algGenetico 30 (blxAlpha 0.3) 1.0 1 (mutEstacionario 0.001) reempEstacionario

  -- Genético estacionario con cruce aritmético
  ageCa :: StdGen -> Algoritmo
  ageCa =  algGenetico 30 (cruceAritmetico 0.5) 1.0 1 (mutEstacionario 0.001) reempEstacionario

  -- Esquema general de un algoritmo genético
  algGenetico :: Int -> OpCruce -> Float -> Int -> EsqMutacion -> EsqReemp -> StdGen -> Algoritmo
  algGenetico nPob opCruce pCruce nParejas esqMutacion esqReemp gen datos = getPesos $ maximum $ evalState (hastaQueM (\_ -> maxIteraciones 15000) generarPob (crearPobIni nPob datos)) (gen, 0)
    where
      generarPob pob =
        do
          padres <- seleccion nParejas pob
          hijos <- cruce pCruce datos opCruce padres
          hMut <- esqMutacion (mutNormal 0.3 datos) hijos
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

  -- Esquema de mutación: versión generacional, fijamos el nº de mutaciones y seleccionamos al azar. Se evita que
  -- un mismo cromosoma mute repetidamente un mismo gen
  mutGeneracional :: Float -> EsqMutacion
  mutGeneracional pMutGen opMut hijos = do
    let nMuts = round $ pMutGen * genericLength hijos * getNGeneric hijos
    iRandoms <- randRs (0, length hijos - 1)
    let croMuts = group $ sort $ take nMuts iRandoms
    foldM (mutarCromosoma opMut) hijos croMuts

  -- Se mutan tantos genes distintos de un cromosoma como nº de veces que haya salido al coger aleatoriamente
  mutarCromosoma :: OpMutacion -> Poblacion -> [Int] -> Estado Poblacion
  mutarCromosoma opCruce pob indices = do
    let cro = pob !! (head indices)
    let nMuts = length indices
    let n = U.length $ getPesos cro
    (_, iGenes) <- repiteNM nMuts (indicesSinRepetir) ([0..(n - 1)], [])
    nuevoCro <- opCruce cro iGenes
    return $ [nuevoCro] ++ (delete cro pob)

  -- Función auxiliar
  indicesSinRepetir :: ([Int], [Int]) -> Estado ([Int], [Int])
  indicesSinRepetir (indices, acc) =
    do
      i <- randR (0, length indices - 1)
      return (delete (indices !! i) indices, acc ++ [indices !! i])

  -- Esquema de mutación: versión estacional, tomamos los dos hijos y vemos la prob de mutación a nivel de cromosoma
  mutEstacionario :: Float -> EsqMutacion
  mutEstacionario pMutGen opMut hijos = do
    let pMutCro = pMutGen * (getNGeneric hijos)
    pRandoms <- randRs (0.0, 1.0)
    iRandoms <- randRs (0, getN hijos - 1)
    let pMut = take 2 pRandoms
    let iGens = take 2 iRandoms
    m1 <- ifM (return $ (pMut !! 0) < pMutCro) (opMut (hijos !! 0) [iGens !! 0]) (return $ hijos !! 0)
    m2 <- ifM (return $ (pMut !! 1) < pMutCro) (opMut (hijos !! 1) [iGens !! 1]) (return $ hijos !! 1)
    return [m1, m2]

  -- Operador de mutación: Mutamos el hijo en las posiciones iGenes que se pasan con desviación estandar sD
  mutNormal :: Float -> Datos -> OpMutacion
  mutNormal sD datos hijo iGenes =
    do
      pesos <- U.imapM (\i x -> ifM (return $ i `elem` iGenes) (mutGen sD x) (return x)) (getPesos hijo)
      crearCromosoma datos pesos

  -- Pasa un gen y lo modifica segun el op de mutación
  mutGen :: Float -> Gen -> Estado Gen
  mutGen sD x = do
    z <- rNormal sD
    return $ min 1.0 $ max 0.0 $ x + z

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
