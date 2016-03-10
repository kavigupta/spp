module Tools.Files(
        eitherHandler,
        sppHandler,
        allFiles,
        realContents,
        actual,
        removeDirectoryIfExists,
        PreprocessorResult(..),
        mapOverSuccess
    ) where

import Interface.Errors
import Control.Exception

import System.Directory

import Control.Monad(forM, when)


data PreprocessorResult = SPPSuccess String |
        SPPFailure SPPError | 
        SPPRequire FilePath (IO PreprocessorResult)

instance Show PreprocessorResult where
    show (SPPSuccess str) = "SPPSuccess " ++ show str
    show (SPPFailure err) = "SPPFailure " ++ show err
    show (SPPRequire path _) = "SPPRequire " ++ path

mapOverSuccess :: (String -> String) -> PreprocessorResult -> PreprocessorResult
mapOverSuccess f (SPPSuccess x) = SPPSuccess . f $ x
mapOverSuccess _ x              = x

-- Handles an error by using the left error reporting mechanism
eitherHandler :: IOExcHandler -> IOException -> IO (Either SPPError a)
eitherHandler handler = return . Left . handler

sppHandler :: IOExcHandler -> IOException -> IO PreprocessorResult
sppHandler handler = return . SPPFailure . handler

allFiles :: FilePath -> IO [FilePath]
allFiles path
        = do
            isdir <- doesDirectoryExist path
            if isdir then subFiles else return [path]
    where
    subFiles :: IO [FilePath]
    subFiles = do
        nonup <- realContents path
        let paths = map ((path ++ "/") ++) nonup
        allSubdrs <- forM paths allFiles
        return $ concat allSubdrs

actual :: String -> Bool
actual x = x /= "." && x /= ".."

realContents :: FilePath -> IO [String]
realContents path = do
    contents <- getDirectoryContents path
    return $ filter actual contents

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists path = do
    exists <- doesDirectoryExist path
    when exists $ removeDirectoryRecursive path 
