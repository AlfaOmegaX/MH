{-|
Module      : Lectura
Author      : Miguel Lentisco Ballesteros
Description : Implementa la lectura y procesado de datos
-}
module Lectura where

  {-# LANGUAGE StrictData, Strict #-}
  import Base
  import Data.List (nub, groupBy, sortBy)
  import qualified Data.Vector.Unboxed as U (fromList, imap, map, zipWith, (!))
  import Data.List.Split (splitOn)

  ---------------------------------------------------------------------------------
  -- Lectura de tipo .arff, normalización y división según clase
  ---------------------------------------------------------------------------------
  -- Crea un dato a partir de una linea del fichero
  creaDato :: String -> Dato
  creaDato linea = (U.fromList $ fmap read $ init vector, filter (/=' ') $ last vector)
    where vector = splitOn "," linea

  -- Lee el archivo y crea una lista con los datos normalizados
  crearDatos :: String -> Datos
  crearDatos contenido = nub $ fmap creaDato rawDatos
      where (_:rawDatos) = dropWhile (/= "@data") $ lines contenido

  -- Lee los datos sin normalizar y los normaliza entre [0,1]
  normalizar :: Datos -> Datos
  normalizar datos =
    let (first:_)    = datos
        maxMin x y   = (fst x `max` y, snd x `min` y)
        inicial      = U.map (\x -> (x, x)) $ fst first
        lMaxMin      = foldl (\acc x -> U.zipWith maxMin acc (fst x)) inicial datos
        maxI         = fst . (lMaxMin U.!)
        minI         = snd . (lMaxMin U.!)
        dif i        = maxI i - minI i
        norValor i x = if dif i == 0 then 0.0 else (x - minI i) / dif i
    in fmap (\x -> (U.imap norValor (fst x), snd x)) datos

  -- Divide los datos según su clase
  separarClases :: Datos -> [Datos]
  separarClases datos = groupBy mismaClase datosOrdenados
    where datosOrdenados = sortBy comparaClase datos
