{-# LANGUAGE DoAndIfThenElse #-}
module FileHandler(
        BackedUpFile(..),
        BackedUpFolder(..),
        createBackups, removeBackups,
        setWritable
    ) where

import System.Directory

import System.Exit

import Control.Monad

data BackedUpFile = BackedUpFile {
    originalFile :: FilePath,
    backupFile :: FilePath
}

data BackedUpFolder = BackedUpFolder {
    originalRoot :: FilePath,
    backedUpRoot :: FilePath,
    backups :: [BackedUpFile]
}

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

actual :: String -> Bool
actual x = x /= "." && x /= ".."

copyDirectories :: FilePath -> FilePath -> IO ()
copyDirectories src dest = do
    contents <- getDirectoryContents src
    dirs <- filterM (doesDirectoryExist . sourcify) $ filter actual contents
    createDirectory dest
    forM_ dirs $ \p -> copyDirectories (sourcify p) (destify p)
        where
        sourcify = ((src ++ "/") ++)
        destify = ((dest ++ "/") ++)

setWritable :: Bool -> FilePath -> IO ()
setWritable b f = do
    p <- getPermissions f
    setPermissions f (p {writable = b})

backupExistsError :: String
backupExistsError = "A backup already exists. Perhaps you forgot to run spp --clean last time"

createBackups :: FilePath -> IO BackedUpFolder
createBackups root = do
        backupExists <- doesFileExist buRoot
        when backupExists $ putStrLn backupExistsError >> exitFailure
        files <- allFiles root
        let newFiles = map (makeBackup root) files
        let backedUpFiles = zipWith BackedUpFile files newFiles
        renameDirectory root buRoot
        copyDirectories buRoot root
        return BackedUpFolder {originalRoot=root, backedUpRoot=buRoot, backups = backedUpFiles}
    where
    buRoot = rootBackup root

{-
Returns whether or not the backup was successfully removed
-}
removeBackups :: FilePath -> IO Bool
removeBackups root = do
        backupExists <- doesDirectoryExist buRoot
        if not backupExists then return False
        else do
            removeDirectoryRecursive root
            renameDirectory buRoot root
            return True
    where buRoot = rootBackup root

makeBackup :: FilePath -> FilePath -> FilePath
makeBackup root file = rootBackup root ++ drop (length root) file

rootBackup :: FilePath -> FilePath
rootBackup = (++ ".bak")
