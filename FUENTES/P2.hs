{-|
Module      : P2
Author      : Miguel Lentisco Ballesteros
Description : P2 ~ Algoritmos genéticos y meméticos
-}
module P2 where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import Utils
  import P1 (busLocMem)
  import qualified Data.Vector.Unboxed as U (Vector, imapM, fromList, foldl, zipWithM, zipWith, empty, length)
  import Data.List (genericLength, sort, delete, maximum, minimum, group)
  import Control.Monad (zipWithM, replicateM, foldM)
  import Control.Monad.Extra (ifM)
  import System.Random (StdGen)
  import Control.Monad.State (evalState)

  algoritmosP2 :: StdGen -> [(String, Algoritmo)]
  --algoritmosP2 gen = [("AGG-BLX", aggBlx gen),("AGG-CA", aggCa gen), ("AGE-BLX", ageBlx gen), ("AGE-CA", ageCa gen)]
  algoritmosP2 gen = [("AM-(10, 1.0)", amTodos gen)]--, ("AM-(10, 0.1)", amProb gen), ("AM-(10, 0.1mej)", amMejor gen)]

  ---------------------------------------------------------------------------------
  -- Algoritmos genéticos
  ---------------------------------------------------------------------------------
  -- Genético generacional con cruce BLX
  aggBlx :: StdGen -> Algoritmo
  aggBlx = esqGenetico noAplica 30 (blxAlpha 0.3) 0.7 15 (mutGeneracional 0.001) reempGeneracional

  -- Genético generacional con cruce aritmético
  aggCa :: StdGen -> Algoritmo
  aggCa =  esqGenetico noAplica 30 (cruceAritmetico 0.5) 0.7 15 (mutGeneracional 0.001) reempGeneracional

  -- Genético estacionario con cruce BLX
  ageBlx :: StdGen -> Algoritmo
  ageBlx = esqGenetico noAplica 30 (blxAlpha 0.3) 1.0 1 (mutEstacionario 0.001) reempEstacionario

  -- Genético estacionario con cruce aritmético
  ageCa :: StdGen -> Algoritmo
  ageCa = esqGenetico noAplica 30 (cruceAritmetico 0.5) 1.0 1 (mutEstacionario 0.001) reempEstacionario

  esqGenetico :: EsqBL -> Int -> OpCruce -> Double -> Int -> EsqMutacion -> EsqReemp -> StdGen -> Algoritmo
  esqGenetico esqBL nPob opCruce pCruce nParejas esqMutacion esqReemp gen datos = getPesos $ maximum $ fst $ evalState (hastaQueM (\_ -> maxIteraciones 15000) generarPob ((\x -> (x, 0 :: Int)) <$> crearPobIni nPob datos)) (gen, 0)
    where
      generarPob (pob, i) =
        do
          padres <- seleccion nParejas pob
          hijos <- cruce pCruce datos opCruce padres
          hMut <- esqMutacion (mutNormal 0.3 datos) hijos
          nuevaPob <- esqReemp pob hMut
          let iNuevo = i + 1
          pobFinal <- ifM (return $ (iNuevo `mod` 10) == 0) (esqBL nuevaPob) (return nuevaPob)
          return (pobFinal, iNuevo)

  -- Esquema BL: No aplica BL (genético normal)
  noAplica :: EsqBL
  noAplica = return

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
  cruce :: Double -> Datos -> EsqCruce
  cruce pCruce datos opCruce padres =
    do
      let nCruces = round $ pCruce * genericLength padres / (2 :: Double)
      let (cruces, noCruce) = splitAt (nCruces * 2) padres
      let (padres1, padres2) = splitAt nCruces cruces
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
  cruceAritmetico :: Double -> OpCruce
  cruceAritmetico alpha p1 p2 = do
    let cLineal i1 i2 = (alpha * i1 + (1 - alpha) * i2, (1 - alpha) * i1 + alpha * i2)
    let (w1, w2) = separarGenes $ U.zipWith cLineal (getPesos p1) (getPesos p2)
    return (w1, w2)

  -- Operador de cruce: blx-alpha
  blxAlpha :: Double -> OpCruce
  blxAlpha alpha p1 p2 =
    do
      res <- U.zipWithM (cruzarGenes alpha) (getPesos p1) (getPesos p2)
      let (w1, w2) = separarGenes res
      return (w1, w2)

  -- Creamos los dos i-esimo gen para blx
  cruzarGenes :: Double -> Gen -> Gen -> Estado (Gen, Gen)
  cruzarGenes alpha i1 i2 =
    do
      let (cMax, cMin) = (max i1 i2, min i1 i2)
      let l = cMax - cMin
      let (a, b) = (cMin - l * alpha, cMax + l * alpha)
      listaRands <- randRs (a, b)
      let (g1:g2:_) = take 2 listaRands
      return (restringe g1, restringe g2)

  -- Función axuliar: transforma un vector de (Gen, Gen) en (Pesos, Pesos)
  separarGenes :: U.Vector (Gen, Gen) -> (Pesos, Pesos)
  separarGenes genes = (U.fromList h1, U.fromList h2)
    where (h1, h2) = U.foldl (\(acc1, acc2) (g1, g2) -> (acc1 ++ [g1], acc2 ++ [g2])) ([], []) genes

  -- Esquema de mutación: versión generacional, fijamos el nº de mutaciones y seleccionamos al azar. Se evita que
  -- un mismo cromosoma mute repetidamente un mismo gen
  mutGeneracional :: Double -> EsqMutacion
  mutGeneracional pMutGen opMut hijos = do
    let nMuts = max 1 $ round $ pMutGen * genericLength hijos * getNGeneric hijos
    iRandoms <- randRs (0, length hijos - 1)
    let croMuts = group $ sort $ take nMuts iRandoms
    foldM (mutarCromosoma opMut) hijos croMuts

  -- Se mutan tantos genes distintos de un cromosoma como nº de veces que haya salido al coger aleatoriamente
  mutarCromosoma :: OpMutacion -> Poblacion -> [Int] -> Estado Poblacion
  mutarCromosoma opCruce pob indices = do
    let cro = pob !! head indices
    let nMuts = length indices
    let n = U.length $ getPesos cro
    (_, iGenes) <- repiteNM nMuts indicesSinRepetir ([0..(n - 1)], [])
    nuevoCro <- opCruce cro iGenes
    return $ nuevoCro:delete cro pob

  -- Función auxiliar
  indicesSinRepetir :: ([Int], [Int]) -> Estado ([Int], [Int])
  indicesSinRepetir (indices, acc) =
    do
      i <- randR (0, length indices - 1)
      return (delete (indices !! i) indices, acc ++ [indices !! i])

  -- Esquema de mutación: versión estacional, tomamos los dos hijos y vemos la prob de mutación a nivel de cromosoma
  mutEstacionario :: Double -> EsqMutacion
  mutEstacionario pMutGen opMut hijos = do
    let pMutCro = pMutGen * getNGeneric hijos
    pRandoms <- randRs (0.0, 1.0)
    iRandoms <- randRs (0, getN hijos - 1)
    let pMut = take 2 pRandoms
    let iGens = take 2 iRandoms
    m1 <- ifM (return $ head pMut < pMutCro) (opMut (head hijos) [head iGens]) (return $ head hijos)
    m2 <- ifM (return $ (pMut !! 1) < pMutCro) (opMut (hijos !! 1) [iGens !! 1]) (return $ hijos !! 1)
    return [m1, m2]

  -- Operador de mutación: Mutamos el hijo en las posiciones iGenes que se pasan con desviación estandar sD
  mutNormal :: Double -> Datos -> OpMutacion
  mutNormal sD datos hijo iGenes =
    do
      pesos <- U.imapM (\i x -> ifM (return $ i `elem` iGenes) (mutGen sD x) (return x)) (getPesos hijo)
      crearCromosoma datos pesos

  -- Pasa un gen y lo modifica segun el op de mutación
  mutGen :: Double -> Gen -> Estado Gen
  mutGen sD x = do
    z <- rNormal sD
    return $ restringe $ x + z

  -- Esquema de reemplazamiento: los hijos reemplazan la población y si el mejor
  -- de la población anterior no está, se reemplaza por el peor hijo
  reempGeneracional :: EsqReemp
  reempGeneracional pActual hijos =
    if mejorP `elem` hijos
      then return hijos
      else return $ delete (minimum hijos) hijos ++ [mejorP]
    where mejorP = maximum pActual

  -- Esquema de reemplazamiento: tipo estacionario - metemos a los dos hijos y se eliminan los 2 peores
  reempEstacionario :: EsqReemp
  reempEstacionario pActual hijos = return $ drop 2 $ sort $ pActual ++ hijos

  ---------------------------------------------------------------------------------
  -- Algoritmos meméticos
  ---------------------------------------------------------------------------------

  amTodos :: StdGen -> Algoritmo
  amTodos gen datos = esqGenetico (aplicaTodos datos) 10 (blxAlpha 0.3) 0.7 5 (mutGeneracional 0.001) reempGeneracional gen datos

  amProb :: StdGen -> Algoritmo
  amProb gen datos = esqGenetico (aplicaProb 0.1 datos) 10 (blxAlpha 0.3) 0.7 5 (mutGeneracional 0.001) reempGeneracional gen datos

  amMejor :: StdGen -> Algoritmo
  amMejor gen datos = esqGenetico (aplicaMejor 0.1 datos) 10 (blxAlpha 0.3) 0.7 5 (mutGeneracional 0.001) reempGeneracional gen datos

  -- Esquema BL: Aplica BL a toda la poblacion
  aplicaTodos :: Datos -> EsqBL
  aplicaTodos = aplicaBL

  -- Esquema BL: Aplica BL al conjunto de cromosomas elegidos por prob
  aplicaProb :: Double -> Datos -> EsqBL
  aplicaProb pLS datos pob =
    do
      pRands <- randRs (0.0, 1.0)
      let probs = take (length pob) pRands
      let toma (accA, accR, i) x = if (probs !! i) < pLS then (accA ++ [x], accR, i + 1) else (accA, accR ++ [x], i + 1)
      let (escogidos, rechazados, _) = foldl toma ([], [], 0) pob
      aplicados <- aplicaBL datos escogidos
      return $ aplicados ++ rechazados

  aplicaMejor :: Double -> Datos -> EsqBL
  aplicaMejor rMejor datos pob =
    do
      let nMejores = max 1 $ round $ rMejor * genericLength pob
      let (rechazados, escogidos) = splitAt (length pob - nMejores) $ sort pob
      aplicados <- aplicaBL datos escogidos
      return $ aplicados ++ rechazados

  -- Aplica BL a un conjunto de cromosomas
  aplicaBL :: Datos -> EsqBL
  aplicaBL datos = mapM (busLocMem datos)
