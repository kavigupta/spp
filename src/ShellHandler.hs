module ShellHandler (inDir, eitherHandler) where

import System.Directory

import Control.Exception

{- Change the working directory to the given path then evaluate the given IO action -}
inDir :: FilePath -> IO a -> IO a
inDir path action = do
    original <- getCurrentDirectory
    setCurrentDirectory path
    finally action $ setCurrentDirectory original

-- Handles an error by using the left error reporting mechanism 
eitherHandler :: IOException -> IO (Either String a)
eitherHandler err = return . Left $ "An error occured in preprocessing: " ++ show err
