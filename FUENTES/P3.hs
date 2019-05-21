{-|
Module      : P3
Author      : Miguel Lentisco Ballesteros
Description : P3 ~ Algoritmos ES, ISL, DE
-}
module P3 where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import Utils
  import P1 (pesosIniRand, obtenerVecino)
  import System.Random (StdGen)
  import Control.Monad.State (evalState)
  import qualified Control.Monad.HT as M (until)
  import Debug.Trace

  -- Lista de algoritmos
  algoritmosP3 :: StdGen -> [(String, Algoritmo)]
  algoritmosP3 gen = [("ES", enfrSim gen)]

  ---------------------------------------------------------------------------------
  -- Enfriamento simulado
  ---------------------------------------------------------------------------------

  -- Enfriamento simulado, seguimos iterando hasta 15k iteraciones o max_exitos = 0
  enfrSim :: StdGen -> Algoritmo
  enfrSim gen datos = getPesos solMej
    where (_, _, solMej, _, _, _) = evalState (hastaQueM criterioParada (iterEnfriamento datos) (solTempInicial 0.3 0.3 datos)) (gen, 0)

  -- Criterio de parada (15k iter o max_exitos = 0)
  criterioParada :: ((Solucion, [Int]), Temp, Solucion, Temp, Temp, Int) -> Estado Bool
  criterioParada ((sol, _), _, _, _, _, nVecExi) = condParada 15000 (sinMejora nVecExi) sol

  -- Iteración principal se hace la busqueda en el vecindario y se enfria la temperatura
  iterEnfriamento :: Datos -> ((Solucion, [Int]), Temp, Solucion, Temp, Temp, Int) -> Estado ((Solucion, [Int]), Temp, Solucion, Temp, Temp, Int)
  iterEnfriamento datos ((solAct, indAct), tAct, mejSol, t0, tf, _) = do
    let nMaxVec = length datos * 10
    --_ <- traceM ("\nmax_vecinos: " ++ show nMaxVec ++ "\nmax_exitos: " ++ show (fromIntegral nMaxVec * 0.1) ++ "\nt0: " ++ show t0 ++ "\ntf: " ++ show tf ++ "\n")
    ((solNuev, indNuev), solMejNueva, nVecGen, nVecExi) <- hastaQueM (condEnfriamento nMaxVec) (exploraVecindario datos tAct) (return ((solAct, indAct), mejSol, 0, 0))
    let m = round $ (15000 :: Double) / fromIntegral nMaxVec
    n <- getIter
    _ <- traceM ("\nNº vecinos generados: " ++ show nVecGen ++ "\nNº vecinos aceptados: " ++ show nVecExi ++ "\nNº de iteraciones: " ++ show n ++ "\nT: " ++ show tAct ++ "\n")
    return ((solNuev, indNuev), enfriaTemp m tAct t0 tf, solMejNueva, t0, tf, nVecExi)

  -- Criterio de enfriamento, Cauchy modificado
  enfriaTemp :: Int -> Temp -> Temp -> Temp -> Temp
  enfriaTemp m t t0 tf = t / (1 + beta * t)
    where beta  = (t0 - tf) / (fromIntegral m * t0 * tf)

  -- Condición de enfriamento (si se sigue buscando en el vecindario): si no se ha sobrepasado los max_vecinos o max_exitos
  condEnfriamento :: Int -> ((Solucion, [Int]), Solucion, Int, Int) -> Estado Bool
  condEnfriamento nMaxVec (_, _, nVec, nVecExi) = return $ nVec >= nMaxVec || nVecExi >= nMaxExi
    where nMaxExi = round $ fromIntegral nMaxVec * (0.1 :: Double)

  -- Búsqueda en el vecindario: se crea un nuevo vecino y si es mejor se acepta; si no se mira la prob segun la temperatura y si se acepta aumenta el n_exitos; en cualquier caso aumenta el n_vecinos
  exploraVecindario :: Datos -> Temp -> ((Solucion, [Int]), Solucion, Int, Int) -> Estado ((Solucion, [Int]), Solucion, Int, Int)
  exploraVecindario datos tAct ((solAct, indAct), mejSol, nVec, nVecExi) = do
    (solNueva, indsNuevos) <- obtenerVecino 0.3 datos indAct solAct
    let indices = if null indsNuevos then [0..(nCaract datos - 1)] else indAct
    let diferencia = getFit solAct - getFit solNueva
    --_ <- traceM ("\nFit actual: " ++ show (getFit solAct) ++ "\nFit nuevo: " ++ show (getFit solNueva) ++ "\n")
    numR <- randR (0.0, 1.0)
    --_ <- traceM ("\nSol iguales?: " ++ show (getPesos solAct == getPesos solNueva))
    --_ <- traceM ("\nProb: " ++ show ( exp (- diferencia / tAct)) ++ "\nDiferencia: " ++ show diferencia ++ "\ntAct:" ++ show tAct ++ "\nNº vec mej: " ++ show nVecExi ++ "\n")
    if (diferencia /= 0) && (diferencia < 0 || numR <= exp (- diferencia / tAct)) then
        return ((solNueva, [0..(nCaract datos - 1)]), max solNueva mejSol, nVec + 1, nVecExi + 1)
    else
      return ((aumentaVecino solAct, indices), mejSol, nVec + 1, nVecExi)

  -- Crea la solución inicial que consta de: (SolAct, indicesAct) + TActual + MejorSol + T0 + Tf
  solTempInicial :: Double -> Temp -> Datos -> Estado ((Solucion, [Int]), Temp, Solucion, Temp, Temp, Int)
  solTempInicial mu phi datos = do
    (solIni, indices) <- M.until (tempInicialValida mu phi . fst) (pesosIniRand datos)
    let tempIni = mu * getFit solIni / (- log phi)
    return ((solIni, indices), tempIni , solIni, tempIni, 0.001, 1)

  -- Comprueba que la temperatura inicial es mayor que la final
  tempInicialValida :: Double -> Double -> Solucion -> Bool
  tempInicialValida mu phi solIni = getFit solIni > - log phi * mu * 0.001
