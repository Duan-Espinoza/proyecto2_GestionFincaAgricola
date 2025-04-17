{-|
Module      : Controllers.Trabajadores
Description : Controlador para validar el acceso de trabajadores y mostrar su información.
Copyright   : (c) 2023
License     : MIT
Maintainer  : geovanniga32@gmail.com
Stability   : experimental
Portability : portable

Este módulo proporciona funciones relacionadas con la validación de acceso de los trabajadores mediante su cédula, así como la visualización de su información detallada. Utiliza los modelos definidos en @Models.Trabajador@.

== Funciones Exportadas

['validarAcceso']
  Valida si un trabajador existe en una lista de trabajadores según su cédula.

['mostrarInformacionTrabajador']
  Muestra en consola la información detallada de un trabajador.
-}

module Controllers.Trabajadores
    ( validarAcceso
    , mostrarInformacionTrabajador
    ) where

import Models.Trabajador
    ( Trabajador(rol, cedula, nombre), buscarTrabajadorPorCedula )

-- | Valida si una cédula ingresada corresponde a un trabajador registrado.
--
-- == Parámetros:
--
-- * @cedulaIngresada@ — Cédula digitada por el usuario.
-- * @lista@ — Lista de trabajadores disponibles.
--
-- == Retorno:
--
-- * 'Just Trabajador' si se encuentra un trabajador con la cédula proporcionada.
-- * 'Nothing' si no se encuentra ningún trabajador con esa cédula.
--
-- Además, imprime un mensaje en consola indicando si el acceso fue concedido o denegado.
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

-- | Muestra por consola la información de un trabajador específico.
--
-- == Parámetros:
--
-- * @t@ — El trabajador cuya información se desea mostrar.
--
-- == Efectos:
--
-- Imprime en pantalla los datos del trabajador: cédula, nombre y rol.
mostrarInformacionTrabajador :: Trabajador -> IO ()
mostrarInformacionTrabajador t = do
    putStrLn "\nInformación del Trabajador:"
    putStrLn $ "Cédula: " ++ cedula t
    putStrLn $ "Nombre: " ++ nombre t
    putStrLn $ "Rol: " ++ rol t
    