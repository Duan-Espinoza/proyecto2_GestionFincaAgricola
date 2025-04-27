{-|
Module      : Controllers.Cosechas
Description : Controlador para gestionar cosechas.
-}

module Controllers.Cosechas (
    registrarCosecha,
    cerrarCosecha,
    consultarCosecha,
    cancelarCosecha
) where

import Models.Cosecha
import Models.Parcela (Parcela, vegetal)
import Models.Trabajador (Trabajador)
import System.IO (hFlush, stdout)
import Data.List (find)
import Data.Maybe (isJust)

-- | Registrar una nueva cosecha
registrarCosecha :: [Cosecha] -> [Trabajador] -> [Parcela] -> IO [Cosecha]
registrarCosecha cosechas trabajadores parcelas = do
    putStrLn "\n--- Registro de Cosecha ---"
    
    -- Solicitar datos al usuario
    trabajadorId <- pedirInput "Ingrese la cédula del trabajador: "
    parcelaId <- pedirInput "Ingrese el código de la parcela: "
    vegetalTipo <- pedirInput "Ingrese el tipo de vegetal: "
    inicio <- pedirInput "Fecha de inicio (YYYY-MM-DD): "
    fin <- pedirInput "Fecha de fin (YYYY-MM-DD): "
    cantidad <- pedirDouble "Cantidad a recolectar (kg): "
    
    -- Validar trabajador y parcela
    let trabajadorValido = any (\t -> cedula t == trabajadorId) trabajadores
    let parcelaValida = find (\p -> codigo p == parcelaId) parcelas
    
    if not trabajadorValido
        then do
            putStrLn "Error: Trabajador no registrado."
            return cosechas
        else case parcelaValida of
            Nothing -> do
                putStrLn "Error: Parcela no existe."
                return cosechas
            Just parcela -> do
                -- Validar tipo de vegetal en la parcela
                if vegetalTipo `notElem` vegetales parcela
                    then do
                        putStrLn "Error: Vegetal no permitido en esta parcela."
                        return cosechas
                    else do
                        -- Validar disponibilidad de fechas
                        let disponible = validarDisponibilidadParcela parcelaId cosechas (inicio, fin)
                        if not disponible
                            then do
                                putStrLn "Error: Parcela ocupada en esas fechas."
                                return cosechas
                            else do
                                -- Generar ID y guardar
                                let nuevaId = generarCosechaId cosechas
                                let nuevaCosecha = Cosecha nuevaId trabajadorId parcelaId inicio fin vegetalTipo cantidad Pendiente
                                putStrLn $ "Cosecha registrada. ID: " ++ nuevaId
                                return (nuevaCosecha : cosechas)

-- Funciones auxiliares para entrada de usuario
pedirInput :: String -> IO String
pedirInput prompt = do
    putStr prompt
    hFlush stdout
    getLine

pedirDouble :: String -> IO Double
pedirDouble prompt = do
    putStr prompt
    hFlush stdout
    readLn