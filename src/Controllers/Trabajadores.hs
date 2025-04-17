module Controllers.Trabajadores
    ( validarAcceso
    , mostrarInformacionTrabajador
    ) where

import Models.Trabajador

-- Valida si un trabajador existe por cédula
validarAcceso :: String -> [Trabajador] -> IO (Maybe Trabajador)
validarAcceso cedulaIngresada lista = do
    let resultado = buscarTrabajadorPorCedula cedulaIngresada lista
    case resultado of
        Just t -> do
            putStrLn $ "\n Acceso concedido: " ++ nombre t ++ " (" ++ rol t ++ ")"
            return (Just t)
        Nothing -> do
            putStrLn "\n Cédula no registrada. Acceso denegado."
            return Nothing

-- Mostrar información detallada de un trabajador
mostrarInformacionTrabajador :: Trabajador -> IO ()
mostrarInformacionTrabajador t = do
    putStrLn "\nInformación del Trabajador:"
    putStrLn $ "Cédula: " ++ cedula t
    putStrLn $ "Nombre: " ++ nombre t
    putStrLn $ "Rol: " ++ rol t
