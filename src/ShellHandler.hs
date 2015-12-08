module ShellHandler (systemInDir, readProcessInDir) where

import System.Directory
import System.Process(readProcess, system)
import System.Exit
import System.FilePath

{- Calls readProcess in the directory containing the given path, passing the path as the first argument -}
readProcessInDir :: FilePath -> String -> String -> IO String
readProcessInDir path toPass str = inDir (takeDirectory path) $ readProcess toPass [path] str

{- Calls system from the given directory -}
systemInDir :: FilePath -> String -> IO ExitCode
systemInDir path toExec = inDir path $ system toExec

{- Change the working directory to the given path then evaluate the given IO action -}
inDir :: FilePath -> IO a -> IO a
inDir path action = do
    original <- getCurrentDirectory
    setCurrentDirectory path
    retval <- action
    setCurrentDirectory original
    return retval
