{-|
Module      : P3
Author      : Miguel Lentisco Ballesteros
Description : P3 ~ Algoritmos ES, ISL, DE
-}
module P3 where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import Utils
  import P1 (crearVecino)
  import qualified Data.List as L (delete, elemIndices)
  import System.Random (StdGen)
  import Control.Monad.State (evalState)
  import qualified Control.Monad.HT as M (until)
  import Control.Monad (replicateM)
  import qualified Data.Vector.Unboxed as U (imap, length, fromList, (!))

  -- Lista de algoritmos
  algoritmosP3 :: StdGen -> [(String, Algoritmo)]
  algoritmosP3 gen = [("ES", eS gen), ("ILS", iLS gen), ("DE-Rand", dERand gen), ("DE-CurrentBest", dECurrentBest gen), ("DE-Best", dEBest gen)]

  ---------------------------------------------------------------------------------
  -- Enfriamento simulado (ES)
  ---------------------------------------------------------------------------------

  -- Enfriamento simulado, seguimos iterando hasta 15k iteraciones o max_exitos = 0
  eS :: StdGen -> Algoritmo
  eS gen datos = getPesos solMej
    where (_, _, solMej, _, _, _) = evalState (hastaQueM criterioParada (iterEnfriamento datos) (solIniES 0.3 0.3 datos)) (gen, 0)

  -- Criterio de parada (15k iter o max_exitos = 0)
  criterioParada :: (Solucion, Temp, Solucion, Temp, Temp, Int) -> Estado Bool
  criterioParada (sol , _, _, _, _, nVecExi) = condParada 15000 (sinMejora nVecExi) sol

  -- Si no ha habido aceptaciones
  sinMejora :: Int -> Solucion -> Bool
  sinMejora nVecExi _ = nVecExi == 0

  -- Crea la solución inicial que consta de: SolAct + + TActual + MejorSol + T0 + Tf
  solIniES :: Double -> Temp -> Datos -> Estado (Solucion, Temp, Solucion, Temp, Temp, Int)
  solIniES mu phi datos = do
    solIni <- M.until (tempInicialValida mu phi) (pesosIniRand datos)
    let tempIni = mu * (100 - getFit solIni) / (- log phi)
    return (solIni, tempIni, solIni, tempIni, 0.001, 1)

  -- Comprueba que la temperatura inicial es mayor que la final
  tempInicialValida :: Double -> Double -> Solucion -> Bool
  tempInicialValida mu phi solIni = getFit solIni > - log phi * mu * 0.001

  -- Iteración principal se hace la busqueda en el vecindario y se enfria la temperatura
  iterEnfriamento :: Datos -> (Solucion, Temp, Solucion, Temp, Temp, Int) -> Estado (Solucion, Temp, Solucion, Temp, Temp, Int)
  iterEnfriamento datos (solAct, tAct, mejSol, t0, tf, _) = do
    let nMaxVec = nCaract datos * 10
    (solNueva, solMejNueva, _, nVecExi) <- hastaQueM (condEnfriamento nMaxVec) (exploraVecindario datos tAct) (return (solAct, mejSol, 0, 0))
    let m = round $ (15000 :: Double) / fromIntegral nMaxVec
    return (solNueva, enfriaCauchy m tAct t0 tf, solMejNueva, t0, tf, nVecExi)

  -- Criterio de enfriamento, Cauchy modificado
  enfriaCauchy :: Int -> Temp -> Temp -> Temp -> Temp
  enfriaCauchy m t t0 tf = t / (1 + beta * t)
    where beta = (t0 - tf) / (fromIntegral m * t0 * tf)

  -- Criterio geométrico
  enfriaGeometrica :: Int -> Temp -> Temp -> Temp -> Temp
  enfriaGeometrica _ t _ _ = t * 0.90

  -- Condición de enfriamento (si se sigue buscando en el vecindario): si no se ha sobrepasado los max_vecinos o max_exitos, o las 15k iteraciones
  condEnfriamento :: Int -> (Solucion, Solucion, Int, Int) -> Estado Bool
  condEnfriamento nMaxVec (_, _, nVec, nVecExi) = do
    b <- maxIteraciones 15000
    return $ b || nVec >= nMaxVec || nVecExi >= nMaxExi
    where nMaxExi = round $ fromIntegral nMaxVec * (0.1 :: Double)

  -- Búsqueda en el vecindario: se crea un nuevo vecino y si es mejor se acepta; si no se mira la prob segun la temperatura y si se acepta aumenta el n_exitos; en cualquier caso aumenta el n_vecinos
  exploraVecindario :: Datos -> Temp -> (Solucion, Solucion, Int, Int) -> Estado (Solucion, Solucion, Int, Int)
  exploraVecindario datos tAct (solAct, mejSol, nVec, nVecExi) = do
    solNueva <- obtenerVecino 0.3 datos solAct
    let diferencia = getFit solAct - getFit solNueva
    let diferencia' = if diferencia == 0 then 0.005 else diferencia
    numR <- randR (0.0, 1.0)
    if diferencia' < 0 || numR <= exp (- diferencia' / tAct) then
      return (solNueva, max solNueva mejSol, nVec + 1, nVecExi + 1)
    else
      return (aumentaVecino solAct, mejSol, nVec + 1, nVecExi)

  -- Obtengo una nueva solución del vecindario de la solución actual
  obtenerVecino :: Double -> Datos -> Solucion -> Estado Solucion
  obtenerVecino sD datos sol = do
    ind <- randR (0, nCaract datos - 1)
    z <- rNormal sD
    let pesosN = U.imap (\i x -> if i == ind then restringe $ x + z else x) $ getPesos sol
    crearSolucion datos pesosN

  ---------------------------------------------------------------------------------
  -- Búsqueda Local Reiterada (ILS)
  ---------------------------------------------------------------------------------

  -- ILS, se evalua 15 veces
  iLS :: StdGen -> Algoritmo
  iLS gen datos = getPesos sol
    where sol = evalState (repiteNM2 14 (iteracionILS datos) (solIniILS datos)) (gen, 0)

  -- Solución inicial de ISL: Aplicar BL a una sol aleatoria
  solIniILS :: Datos -> Estado Solucion
  solIniILS datos = do
    sol <- pesosIniRand datos
    blILS datos sol

  -- BL para ISL
  blILS :: Datos -> Solucion -> Estado Solucion
  blILS datos sol = fst <$> repiteNM2 1000 (crearVecino datos) (return (sol, [0..(nCaract datos -1)]))

  -- Se coge la sol actual, se muta y se aplica BL; se devuelve la mejor solución de las dos
  iteracionILS :: Datos -> Solucion -> Estado Solucion
  iteracionILS datos solAct = do
    solMutada <- mutarSolILS datos solAct
    solBL <- blILS datos solMutada
    return (max solAct solBL)

  -- Toma una solucion y muta el 10% de sus pesos
  mutarSolILS :: Datos -> Solucion -> Estado Solucion
  mutarSolILS datos solAct = do
    let nMutaciones = min 1 $ round $ (0.1 :: Double) * fromIntegral (nCaract datos)
    pesosMutados <-repiteNM nMutaciones (mutarPesos 0.4) (getPesos solAct)
    crearSolucion datos pesosMutados

  -- Muta unos pesos
  mutarPesos :: Double -> Pesos -> Estado Pesos
  mutarPesos sD pesos = do
    ind <- randR (0, U.length pesos - 1)
    z <- rNormal sD
    return $ U.imap (\i x -> if i == ind then restringe $ x + z else x) pesos

  ---------------------------------------------------------------------------------
  -- Evolución diferencial (DE)
  ---------------------------------------------------------------------------------

  -- Evolución diferencial con mutación aleatoria
  dERand :: StdGen -> Algoritmo
  dERand = dE mutRand

  -- Evolución diferencial con mutación el actual hacia el mejor
  dECurrentBest :: StdGen -> Algoritmo
  dECurrentBest = dE mutCurrentBest

  -- Evolución diferencial con mutación el mejor
  dEBest :: StdGen -> Algoritmo
  dEBest = dE mutBest

  -- Esquema general de evolución diferencial
  dE :: EsqMutar -> StdGen -> Algoritmo
  dE esqMut gen datos = getPesos (maximum pobSol)
    where pobSol = evalState (hastaQueM (\_ -> maxIteraciones 15000) (iterDE datos esqMut) (crearPobIni 50 datos)) (gen, 0)

  -- Crea nPob individuos aleatorios
  crearPobIni :: Int -> Datos -> Estado Poblacion
  crearPobIni nPob datos = replicateM nPob (crearCromIni datos)

  -- Crea un cromosoma aleatorio
  crearCromIni :: Datos -> Estado Cromosoma
  crearCromIni datos =
    do
      nRandoms <- randRs (0.0, 1.0)
      let pesos = U.fromList $ take (nCaract datos) nRandoms
      crearCromosoma datos pesos

  -- Actualizamos la población para todos los cromosomas
  iterDE :: Datos -> EsqMutar -> Poblacion -> Estado Poblacion
  iterDE datos esqMutRecom pobActual = do
    (pobNueva, _) <- repiteNM (length pobActual) (actualizarPoblacion datos esqMutRecom) (pobActual, 0)
    return pobNueva

  -- Iteración i-ésima, actualiza el miembro i de la población
  actualizarPoblacion :: Datos -> EsqMutar -> (Poblacion, Int) -> Estado (Poblacion, Int)
  actualizarPoblacion datos esqMut (pob, i) = do
    let vectorIni = replicate (nCaract datos) 0.0
    indices <- tomaIndRand 3 (L.delete i [0..(length pob - 1)])
    (pesosNuevos, _) <- repiteNM (nCaract datos) (mutReemp esqMut pob i indices) (vectorIni, 0)
    let pesosNuevos' = U.fromList pesosNuevos
    hijoNuevo <- crearCromosoma datos pesosNuevos'
    let pobActualizada = if hijoNuevo > (pob !! i) then take i pob ++ [hijoNuevo] ++ drop (i + 1) pob else pob
    return (pobActualizada, i + 1)

  -- Iteración de mutar y reemplazar para el elemento i en la característica j
  mutReemp :: EsqMutar -> Poblacion -> Int -> [Int] -> ([Double], Int) -> Estado ([Double], Int)
  mutReemp esqMut pob i indices (pesos, j) = do
    r <- randR (0.0, 1.0)
    valorModif <- if r <= (0.5 :: Double) then esqMut pob i j indices else return (getPesos (pob !! i) U.! j)
    return (take j pesos ++ [valorModif] ++ drop (j + 1) pesos, j + 1)

  -- Mutación aleatoria: toma 3 padres distintos (y de i) y combina los valores de la característica j
  mutRand :: EsqMutar
  mutRand pob _ j (i1:i2:i3:_) = return $ restringe $ getPesos (pob !! i1) U.! j + 0.5 * (getPesos (pob !! i2) U.! j - getPesos (pob !! i3) U.! j)

  -- Mutación actual al mejor: diferencia entre 2 aleatorios y el actual y el mejor
  mutCurrentBest :: EsqMutar
  mutCurrentBest pob i j (i1:i2:_) = do
    let (iMejor:_) = maximum pob `L.elemIndices` pob
    return $ restringe $ getPesos (pob !! i) U.! j + 0.5 * (getPesos (pob !! iMejor) U.! j - getPesos (pob !! i) U.! j) + 0.5 * (getPesos (pob !! i1) U.! j - getPesos (pob !! i2) U.! j)

  -- Mutación mejor: diferencia entre 2 aleatorios y suma el mejor
  mutBest :: EsqMutar
  mutBest pob _ j (i1:i2:_) = do
    let (iMejor:_) = maximum pob `L.elemIndices` pob
    return $ restringe $ getPesos (pob !! iMejor) U.! j + 0.5 * (getPesos (pob !! i1) U.! j - getPesos (pob !! i2) U.! j)

  -- Toma nInd indices distintos de la lista de índices
  tomaIndRand :: Int -> [Int] -> Estado [Int]
  tomaIndRand nInd indices = snd <$> repiteNM nInd tomaIndice (indices, [])
    where
      tomaIndice (inds, res) =
        do
          i <- randR (0, length inds - 1)
          return (L.delete (inds !! i) inds, res ++ [inds !! i])
