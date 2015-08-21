module Arguments (
Function,
FunctionsMap,
process
) where

import System.Environment


type Function = [String] -> IO ()
type FunctionsMap = [(String, Function)]


process :: FunctionsMap -> IO ()
process map = do
    args <- getArgs
    case dispatch map args of
        Nothing -> putStrLn "No action given or action incorrect"
        Just io -> io


dispatch :: FunctionsMap -> [String] -> Maybe (IO ())
dispatch _ [] = Nothing
dispatch map (action:args) =
    lookup action map
    >>= \actionFunction -> Just (actionFunction args)

