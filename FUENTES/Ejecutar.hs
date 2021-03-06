{-|
Module      : Ejecutar
Author      : Miguel Lentisco Ballesteros
Description : Implementa para poder ejecutar los algoritmos y obtener resultados
-}
module Ejecutar where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import KNN
  import P1 (algoritmosP1)
  import P2 (algoritmosP2)
  import P3 (algoritmosP3)
  import qualified Data.Vector.Unboxed as U (length, filter)
  import System.Random (StdGen)
  import System.Clock (Clock(Realtime), getTime, toNanoSecs)
  import Control.DeepSeq (deepseq)

  practicaActual :: Int -> Int
  practicaActual i = i - 1

  -- Todos los algoritmos
  algoritmos :: StdGen -> [[(String, Algoritmo)]]
  algoritmos gen = [algoritmosP1 gen, algoritmosP2 gen, algoritmosP3 gen]

  ---------------------------------------------------------------------------------
  -- Evaluación de algoritmos
  ---------------------------------------------------------------------------------
  -- Ejecuta todos los algoritmos que se tienen y devuelve el resultado de todos
  ejecutarAlgoritmos :: Particiones -> StdGen -> IO String
  ejecutarAlgoritmos particiones gen = do
      resultados <- mapM (ejecuta particiones) (algoritmos gen !! (practicaActual 3))
      return $ init $ init $ unlines resultados

  -- Ejecuta los algoritmos en cada partición y devuelve un mensaje con todos los resultados
  ejecuta :: Particiones -> (String, Algoritmo) -> IO String
  ejecuta particiones (nomAlg, alg) =
    do
      resultados <- mapM (obtenerResultados alg) particiones
      let nombreAlgoritmo             = "Algoritmo " ++ nomAlg ++ ":\n"
      let (tAci, tRed, t, n, mensaje) = foldl crearInfo (0.0, 0.0, 0.0, 0, nombreAlgoritmo) resultados
      let aciMedio                    = tAci / fromIntegral n
      let redMedio                    = tRed / fromIntegral n
      let tMedio                      = t / fromIntegral n
      return $ mensaje ++ "media & " ++ show aciMedio ++ " & " ++ show redMedio ++ " & " ++ show (fEvaluacion 0.5 aciMedio redMedio) ++ " & " ++ show tMedio ++ "\n"

  -- Formateo de mensaje de salida
  crearInfo :: (Double, Double, Double, Int, String) -> (Double, Double, Double) -> (Double, Double, Double, Int, String)
  crearInfo (a, r, t, i, c)  (x, y, z) =
      let mensaje = show (i + 1) ++ " & " ++ show x ++ " & " ++ show y ++ " & " ++ show (fEvaluacion 0.5 x y) ++  " & " ++ show z ++ "\n"
      in (a + x, r + y, t + z, i + 1, c ++ mensaje)

  -- Aplica un algoritmo para obtener pesos y devuelve los resultados junto con el tiempo tardado
  obtenerResultados :: Algoritmo -> Particion -> IO (Double, Double, Double)
  obtenerResultados alg (train, test) =
    do
      (pesos, tiempo) <- pesosTiempo alg train
      let pReduccion = fromIntegral (U.length (U.filter (< 0.2) pesos)) / fromIntegral (U.length pesos)
      let pAcierto = clas1nn train test $ reducePesos pesos
      return (100.0 * pAcierto, 100.0 * pReduccion, tiempo)

  -- Aplica el algoritmo para obtener pesos y devuelve los pesos y el tiempo tardado
  pesosTiempo :: Algoritmo -> Datos -> IO (Pesos, Double)
  pesosTiempo alg train =
    do
      t1 <- getTime Realtime
      let pesos = alg train
      t2 <- pesos `deepseq` getTime Realtime
      return (pesos, fromIntegral(toNanoSecs(t2 - t1)) / 1000000000)
