module FileHandler(
        allFiles, setWritable, createBackups
    ) where

import Data.List
import System.Directory
import System.Exit
import Control.Monad
import Control.Applicative
import ArgumentProcessor

allFiles :: FilePath -> IO [FilePath]
allFiles path = do
    isdir <- doesDirectoryExist path
    if isdir then subFiles path else return [path]

subFiles :: FilePath -> IO [FilePath]
subFiles path = do
    contents <- getDirectoryContents path
    putStrLn $ show contents
    let nonup = filter actual contents
    putStrLn $ show nonup
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
        forM_ files $ \path -> renameFile path (createBackup path)
    where
    createBackup = (++ ".bak")
    backups = map createBackup files
