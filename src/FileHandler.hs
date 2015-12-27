{-# LANGUAGE DoAndIfThenElse #-}
module FileHandler(
        BackedUpFile(..),
        createBackups, removeBackups,
        setWritable
    ) where

import Tools.Files
import Interface.Errors

import System.Directory
import Control.Exception

import Shelly.Lifted(cp_r, fromText, shelly)

import Data.Text(pack)
import Data.Function(on)
import Control.Monad(liftM2)
import Control.Applicative((<$>))

data BackedUpFile = BackedUpFile {
    originalFile :: FilePath,
    backupFile :: FilePath
}

copyDirectories :: IOExcHandler -> FilePath -> FilePath -> IO (Either SPPError ())
copyDirectories handler src dst = unsafeCp `catch` eitherHandler handler
    where
    unsafeCp = Right <$> shelly ((cp_r `on` fromText . pack) src dst)

setWritable :: Bool -> FilePath -> IO ()
setWritable b f = do
    p <- getPermissions f
    setPermissions f (p {writable = b})

createBackups :: FilePath -> IO (Either SPPError [BackedUpFile])
createBackups root = do
        backupExists <- liftM2 (||) (doesFileExist buRoot) (doesDirectoryExist buRoot)
        if backupExists then
            return . Left $ SPPError BackupExistsError root Nothing Nothing
        else do
            files <- allFiles root
            let newFiles = map (makeBackup root) files
            let backedUpFiles = zipWith BackedUpFile files newFiles
            renameDirectory root buRoot
            copySuccess <- copyDirectories (sppError BackupError root Nothing) buRoot root
            return $ fmap (const backedUpFiles) copySuccess
    where
    buRoot = rootBackup root

{-
Returns whether or not the backup was successfully removed
-}
removeBackups :: RestoreSituation -> FilePath -> IO (Either SPPError ())
removeBackups situation root = do
        backupExists <- doesDirectoryExist buRoot
        if not backupExists then
            return . Left $ SPPError (RestoreError situation) buRoot Nothing Nothing
        else
            dorestore `catch` eitherHandler (sppError (RestoreError situation) buRoot Nothing)
    where
    buRoot = rootBackup root
    dorestore = do
        removeDirectoryRecursive root
        renameDirectory buRoot root
        return $ Right ()

makeBackup :: FilePath -> FilePath -> FilePath
makeBackup root file = rootBackup root ++ drop (length root) file

rootBackup :: FilePath -> FilePath
rootBackup = (++ ".bak")
