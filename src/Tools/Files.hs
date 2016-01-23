module Tools.Files(
        eitherHandler,
        allFiles,
        realContents,
        actual
    ) where

import Interface.Errors
import Control.Exception

import System.Directory

import Control.Monad(forM)

-- Handles an error by using the left error reporting mechanism
eitherHandler :: IOExcHandler -> IOException -> IO (Either SPPError a)
eitherHandler handler err = return . Left . handler $ err

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
