{-|
Module      : P2
Author      : Miguel Lentisco Ballesteros
Description : P2 ~ Algoritmos genéticos y meméticos
-}
module P2 where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import KNN
  import qualified Data.SortedList as SL (SortedList, fromSortedList, toSortedList, insert, union, drop)
  import qualified Data.Vector.Unboxed as U (Vector, imap, fromList, foldl, zipWithM, zipWith)
  import Data.Ord (comparing)

  -- Un gen es un valor real
  type Gen = Float
  -- Un cromosoma será una solución (pesos) con el valor de la función objetivo (de menor a mayor)
  data Cromosoma = Cromosoma {
    getPesos :: Pesos, -- La solución propiamente dicha: el vector de pesos
    getFit   :: Float  -- El valor de la función objetivo para este cromosoma
  }

  instance Eq Cromosoma where
    _ == _ = False
  -- Ordenamos las soluciones según el valor de la función objetivo
  instance Ord Cromosoma where
    compare = comparing getFit
  -- Una población es una lista de cromosomas ordenadas por su fit
  type Poblacion = SL.SortedList Cromosoma
  -- El operador de cruce toma los 2 padres y devuelve 2 pesos para convertir en hijos
  type OpCruce = Cromosoma -> Cromosoma -> Estado (Cromosoma, Cromosoma)
  -- El operador de mutación toma el hijo y la posición i-ésima donde tiene que mutar y devuelve un nuevo cromosoma
  type OpMutacion = Cromosoma -> Int -> Estado Cromosoma
  -- El esquema de reemplazamiento toma la población actual, los hijos y devuelve la nueva población reemplazada
  type EsqReemplazamiento = Poblacion -> Poblacion -> Poblacion

  -- Crear cromosoma
  crearCromosoma :: Datos -> Pesos -> Cromosoma
  crearCromosoma datos pesos = Cromosoma pesos (evaluarF datos pesos)

  -- Esquema de reemplazamiento: tipo generacional (elitista) si todos los hijos reemplazan quito al peor hijo por el mejor de la población si no reemplazan todos se rellenan con los mejores de la población
  -- TODO: el size, last, tail... para SortedList ??
  reempGeneracional :: EsqReemplazamiento
  reempGeneracional pActual hijos = if (findIndices (\c -> c == mejorP) hijos) == [] then SL.union hijos else hijos
    where mejorP = last $ fromSortedList pActual

  -- Esquema de reemplazamiento: tipo estacionario - metemos a los dos hijos y se eliminan los 2 peores (pueden ser los hijos)
  -- TODO: drop
  reempEstacionario :: EsqReemplazamiento
  reempEstacionario pActual hijos = SL.drop 2 $ SL.union pActual hijos

  -- Operador de mutación: Mutamos el hijo en la posición gen-ésima con desviación estandar sD
  mutarCromosoma :: Float -> Datos -> OpMutacion
  mutarCromosoma sD datos hijo gen =
    do
      z <- rNormal sD
      let pesos = U.imap (\i x -> if i == gen then min 1.0 (max 0.0 (x + z)) else x) (getPesos hijo)
      return $ crearCromosoma datos pesos

  -- Operador de cruce: cruce aritmético (combinación lineal)
  cruceAritmetico :: Float -> Datos -> OpCruce
  cruceAritmetico alpha dataS p1 p2 = do
    let cLineal i1 i2 = (alpha * i1 + (1 - alpha) * i2, (1 - alpha) * i1 + alpha * i2)
    let (w1, w2) = separarGenes $ U.zipWith cLineal (getPesos p1) (getPesos p2)
    return (crearCromosoma dataS w1, crearCromosoma dataS w2)

  -- Operador de cruce: blx-alpha
  blxAlpha :: Float -> Datos -> OpCruce
  blxAlpha alpha dataS p1 p2 =
    do
      res <- U.zipWithM (cruzarGenes alpha) (getPesos p1) (getPesos p2)
      let (w1, w2) = separarGenes res
      return (crearCromosoma dataS w1, crearCromosoma dataS w2)

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
