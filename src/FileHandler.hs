module FileHandler(
        allFiles, setWritable, createBackups, removeGenerated,
        makeBackup, isBackup,
        removeIfExists, handleExists
    ) where

import Data.List
import System.Directory
import System.Exit
import Control.Monad

import Control.Exception
import System.IO.Error

allFiles :: FilePath -> IO [FilePath]
allFiles path = do
    isdir <- doesDirectoryExist path
    if isdir then subFiles path else return [path]

subFiles :: FilePath -> IO [FilePath]
subFiles path = do
    contents <- getDirectoryContents path
    let nonup = filter actual contents
    let paths = map ((path ++ "/") ++) nonup
    allSubdrs <- forM paths allFiles
    return $ concat allSubdrs
        where actual x = x /= "." && x /= ".."

setWritable :: Bool -> FilePath -> IO ()
setWritable b f = do
    p <- getPermissions f
    setPermissions f (p {writable = b})

createBackups :: [FilePath] -> IO ()
createBackups files = do
        bakExists <- forM backups doesFileExist
        let fWithBackup = zip bakExists files
        let existingBak = find fst fWithBackup
        case existingBak of
            Just (_, path)
                -> putStrLn ("A backup exists for file "
                    ++ path
                    ++ "; You probably forgot to run 'gpp --clean' last time") >>
                    exitFailure
            Nothing -> return ()
        forM_ files $ \path -> renameFile path (makeBackup path)
    where
    backups = map makeBackup files

removeGenerated :: [FilePath] -> IO [FilePath]
removeGenerated contents = do
    let files = filter (not . isBackup) contents
    bakExists <- forM (map makeBackup files) doesFileExist
    let fWithBackup = zip bakExists files
    let existingBak = find (not . fst) fWithBackup
    case existingBak of
        (Just (_, path)) -> putStrLn ("No backup exists for file " ++ path) >> exitFailure
        Nothing -> return ()
    return files

isBackup :: FilePath -> Bool
isBackup = isSuffixOf ".bak"

makeBackup :: FilePath -> FilePath
makeBackup = (++ ".bak")

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists

handleExists :: IOError -> IO ()
handleExists e
    | isDoesNotExistError e = return ()
    | otherwise = throwIO e
