{-|
Module      : CrossV
Author      : Miguel Lentisco Ballesteros
Description : Implementa crossvalidation
-}
module CrossV where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import Data.List.Split (chunksOf)
  import Data.List (union, delete)
  import System.Random (StdGen)
  import System.Random.Shuffle (shuffle')
  ---------------------------------------------------------------------------------
  -- k-fold cross-validation
  ---------------------------------------------------------------------------------
  -- Crea las tuplas de particiones con (ConjuntoEntrenamiento, ConjuntoTest)
  eleccionCruzada :: StdGen -> Int -> [Datos] -> Particiones
  eleccionCruzada gen k datos =
    let parts = crearParticiones gen k datos
    in  foldl (\acc x -> acc ++ [(concat (delete x parts), x)]) [] parts

  -- pre: nº datos de cada clase > k
  -- Toma las distintas listas de datos de clases y forma las k particiones
  crearParticiones :: StdGen -> Int -> [Datos] -> [Datos]
  crearParticiones gen k datos =
    let (particiones, resto) = foldl (uneParticiones k) (replicate k [], []) datos
        restoMezclado = if resto == [] then [] else shuffle' resto (length resto) gen
        (particiones', resto') = if length restoMezclado < k then (particiones, restoMezclado) else uneParticiones k (particiones, []) restoMezclado
    in mergeResto resto' particiones'

  -- Junta todas las k particiones de cada clase y una lista de todos los restos
  uneParticiones :: Int -> ([Datos], Datos) -> Datos -> ([Datos], Datos)
  uneParticiones k (acc1, acc2) listaClase = (zipWith union acc1 particiones, union acc2 resto)
    where (particiones, resto) = divideClases k listaClase

  -- Divide la lista de una clase en k particiones y el resto
  divideClases :: Int -> Datos -> ([Datos], Datos)
  divideClases k listaClase = if length particiones == k then (particiones, []) else (init particiones, last particiones)
    where particiones = chunksOf (length listaClase `quot` k) listaClase

  -- Reparte uniformemente los restos de hacer particiones
  mergeResto :: Datos -> [Datos] -> [Datos]
  mergeResto [] xs = xs
  mergeResto (x:lResto) (y:parts) = (y ++ [x]) : mergeResto lResto parts
