{-|
Module      : main
Author      : Miguel Lentisco Ballesteros
Description : Main
-}
module Main where

  {-# LANGUAGE StrictData, Strict #-}
  import Lectura
  import CrossV
  import Ejecutar
  import System.Directory (doesFileExist)
  import System.Environment (getArgs)
  import System.FilePath.Posix (takeBaseName)
  import System.Random (mkStdGen, getStdGen, StdGen)
  import System.Random.Shuffle (shuffle')

  ---------------------------------------------------------------------------------
  -- Main
  ---------------------------------------------------------------------------------
  -- Main
  main :: IO ()
  main = do
    args <- getArgs
    (nombreFichero, gen) <- if length args == 2 then do
      putStrLn "Seed fijada."
      let (nombreFichero:seed:_) = args
      return (nombreFichero, mkStdGen (read seed :: Int))
    else if length args == 1 then do
        putStrLn "Seed aleatoria."
        let (nombreFichero:_) = args
        gen <- getStdGen
        return (nombreFichero, gen)
      else do
        putStrLn "Seed 225938972."
        let gen = mkStdGen 225938971
        return ("", gen)
    if nombreFichero == "" then do
      putStrLn $ "Ejecutando todos los archivos con seed " ++ show gen
      ejecutarTodos gen
    else do
      putStrLn $ "Ejecutando archivo " ++ show nombreFichero ++ " con seed " ++ show gen
      ejecutarPrograma gen nombreFichero
    putStrLn "Finalizando programa."

  -- Ejecuta todos los dataset
  ejecutarTodos :: StdGen -> IO ()
  ejecutarTodos gen = do
    let nombresFicheros = ["ionosphere.arff"]--, "colposcopy.arff", "texture.arff"]
    mapM_ (ejecutarPrograma gen) nombresFicheros

  -- Ejecuta un dataset
  ejecutarPrograma :: StdGen -> String -> IO ()
  ejecutarPrograma gen nombreFichero = do
    fileExits <- doesFileExist nombreFichero
    if fileExits then do
      contenido <- readFile nombreFichero
      let dLeidos = crearDatos contenido
      let dNormalizados = normalizar dLeidos
      let dMezclados = shuffle' dNormalizados (length dNormalizados) gen
      let dClasificados = separarClases dMezclados
      let particiones = eleccionCruzada gen 5 dClasificados
      resultado <- ejecutarAlgoritmos particiones gen
      let ficheroResultado = takeBaseName nombreFichero ++ "_resultados.txt"
      writeFile ficheroResultado resultado
      putStrLn $ "Leído fichero " ++ nombreFichero ++ " sin errores."
    else putStrLn $ "Error: no se encuentra un archivo con el nombre " ++ nombreFichero
