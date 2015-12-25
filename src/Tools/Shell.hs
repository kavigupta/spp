module Tools.Shell (inDir) where

import System.Directory

import Control.Exception

{- Change the working directory to the given path then evaluate the given IO action -}
inDir :: FilePath -> IO a -> IO a
inDir path action = do
    original <- getCurrentDirectory
    setCurrentDirectory path
    finally action $ setCurrentDirectory original
