import Data.Time.Clock --Esto nos proporciona las funciones para trabajar con tiempo
import Data.List --Esto nos permite trabajar con funciones de listas
import System.IO --Permite operaciones de entrada y salida como lectura y escritura de archivos en Haskell
import Control.Exception--Proporciona funciones para manejar excepciones en Haskell
import Control.DeepSeq (deepseq) --Proporciona la función `deepseq` para forzar la evaluación completa de estructuras de datos-}

-- Definimos la estructura de los datos del estudiante
data Universitario = Universitario {
    id :: String, --Definimos el ID del estudiante como un String
    entrada :: UTCTime, --Definimos la hora de entrada del estudiante como un UTCTime
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la universidad o ya salió
} deriving (Show, Read) --Permite convertir automáticamente entre Estudiante y String

-- Función para registrar la entrada del estudiante a la universidad
registrarEntrada :: String -> UTCTime -> [Universitario] -> [Universitario] 
registrarEntrada idEstudiante tiempo universidad =
    Universitario idEstudiante tiempo Nothing : universidad

-- Función para registrar la salida del estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Universitario] -> [Universitario] 
registrarSalida idEstudiante tiempo universidad =
    map (\e -> if idEstudiante == Main.id e then e { salida = Just tiempo } else e) universidad --Usamos Main.id para evitar conflictos con la función id de Haskell

-- Función para buscar al estudiante mediante su ID.
buscarEstudiante :: String -> [Universitario] -> Maybe Universitario 
buscarEstudiante idEstudiante universidad =
    find (\e -> idEstudiante == Main.id e && isNothing (salida e)) universidad
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que el estudiante ha estado en la universidad.
tiempoEnUniversidad :: Universitario -> IO NominalDiffTime 
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función que va guardando la informacion de los estudiantes en el archivo texto BaseDatos_EafitUniversity.txt, previamente creado.
guardarUniversidad :: [Universitario] -> IO ()
guardarUniversidad universidad = do
    withFile "BaseDatos_EafitUniversity.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante universidad))
    putStrLn "Registro guardado en el archivo BaseDatos_EafitUniversity.txt."

-- Función para cargar la informacion del archivo de texto BaseDatos_EafitUniversity.txt.
cargarUniversidad :: IO [Universitario]
cargarUniversidad = do
    contenido <- withFile "BaseDatos_EafitUniversity.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Universitario

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Universitario -> String
mostrarEstudiante (Universitario id entrada salida) =
    "Estudiante {id = \"" ++ id ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Universitario] -> IO ()
listarEstudiantes [] = putStrLn "Actualmente no se encuentran estudiantes en la universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar la universidad desde el archivo de texto
    universidad <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de Registro de Estudiantes!"

    -- Ciclo principal del programa
    cicloPrincipal universidad

-- Función para el ciclo principal del programa
cicloPrincipal :: [Universitario] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarEntrada idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ idEstudiante ++ " registrado en la universidad."
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada

        "2" -> do
            putStrLn "Ingrese el ID del estudiante que sale:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarSalida idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ idEstudiante ++ " ha salido de la universidad."
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            idEstudiante <- getLine
            case buscarEstudiante idEstudiante universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con ID " ++ idEstudiante ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad

--Y eso fue todo, espero le haya gustado nuestra implementación para este programa de gestión de estudiantes en la universidad.🫡