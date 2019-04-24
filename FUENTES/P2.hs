{-|
Module      : P2
Author      : Miguel Lentisco Ballesteros
Description : P2 ~ Algoritmos genéticos y meméticos
-}
module P2 where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import qualified Data.SortedList as SL (toSortedList, insert, union)
  import qualified Data.Vector.Unboxed as U (imap, fromList, foldl, zipWithM, zipWith)
  import System.Random (next, randomRs)
  import Data.Random.Normal (normal')
  import Control.Monad.State (get, put)

  -- Un gen es un valor real
  type Gen = Float
  -- Un cromosoma será una solución (pesos) con el valor de la función objetivo (de menor a mayor)
  data Cromosoma = Cromosoma {
    getPesos :: Pesos -- La solución propiamente dicha: el vector de pesos
    getFit   :: Float -- El valor de la función objetivo para este cromosoma
  } deriving (Eq)
  -- Ordenamos las soluciones según el valor de la función objetivo
  instance Ord Cromosoma where
    compare = compairing getFit
  -- Una población es una lista de cromosomas ordenadas por su fit
  type Poblacion = SL.SortedList Cromosoma
  -- El operador de cruce toma los 2 padres y devuelve varios hijos
  type OpCruce = Cromosoma -> Cromosoma -> Estado Poblacion
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
  reempGeneracional pActual hijos = if nPob == nHij then SL.insert (last pActual) (tail hijos) else SL.union hijos (drop (nPob - nHij) pActual)
      where (nPob, nHij) = (length pActual, length hijos)

  -- Esquema de reemplazamiento: tipo estacionario - metemos a los dos hijos y se eliminan los 2 peores (pueden ser los hijos)
  -- TODO: drop
  reempEstacionario :: EsqReemplazamiento
  reempEstacionario pActual hijos = drop 2 $ SL.union pActual hijos

  -- Operador de mutación: Mutamos el hijo en la posición gen-ésima con desviación estandar sD
  -- TODO: Simplificar el put / get
  mutarCromosoma :: Float -> OpMutacion
  mutarCromosoma sD hijo gen =
    do
      (g, nIter) <- get
      let (v, g') = min 1.0 $ max 0.0 $ normal' (0.0, sD) g
      put (g', nIter)
      return U.imap (\i x -> if i == gen then x + v else x) hijo

  -- Operador de cruce: cruce aritmético (combinación lineal)
  cruceAritmetico :: Float -> OpCruce
  cruceAritmetico alpha p1 p2 = return $ separarHijos $ U.zipWith (cLineal alpha i1 i2) p1 p2
    where cLineal alpha i1 i2 = (alpha * i1 + (1 - alpha) * i2, (1 - alpha) * i1 + alpha * i2)

  -- Operador de cruce: blx-alpha
  blxAlpha :: Float -> OpCruce
  blxAlpha alpha p1 p2 =
    do
      res <- U.zipWithM (crearGen alpha) p1 p2
      return $ separarHijos res

  -- Creamos los dos i-esimo gen para blx
  -- TODO: Simplificar el put/get
  crearGen :: Float -> Gen -> Gen -> Estado (Gen, Gen)
  crearGen alpha i1 i2 =
    do
      let (cMax, cMin) = (max i1 i2, min i1 i2)
      let l = cMax - cMin
      let (a, b) = (cMin - l * alpha, cMax + l * alpha)
      (g, nIter) <- get
      let [g1, g2] = take 2 $ randomRs (a, b) g
      put (snd $ next g, nIter)
      return (g1, g2)

  -- Aux
  separarHijos :: Vector (Gen, Gen) -> Poblacion
  separarHijos genes = SL.toSortedList [U.fromList h1, U.fromList h2]
    where (h1, h2) = U.foldl (\(acc1, acc2) (g1, g2) -> (acc1 ++ g1, acc2 ++ g2)) ([], []) genes
