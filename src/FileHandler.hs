{-# LANGUAGE DoAndIfThenElse #-}
module FileHandler(
        BackedUpFile(..),
        createBackups, removeBackups,
        setWritable
    ) where

import Tools.Files
import Interface.Errors
import Interface.Args

import System.Directory
import System.FilePath

import Control.Exception

import Shelly.Lifted(cp_r, fromText, shelly)

import Data.Text(pack)
import Data.Function(on)
import Control.Monad(liftM2)
import Control.Applicative((<$>))

data BackedUpFile = BackedUpFile {
    originalFile :: FilePath,
    backupFile :: FilePath,
    outputFile :: FilePath
}

copyDirectories :: IOExcHandler -> FilePath -> FilePath -> IO (Either SPPError ())
copyDirectories handler src dst = unsafeCp `catch` eitherHandler handler
    where
    unsafeCp = Right <$> shelly ((cp_r `on` fromText . pack) src dst)

setWritable :: Bool -> FilePath -> IO ()
setWritable b f = do
    p <- getPermissions f
    setPermissions f (p {writable = b})

createBackups :: Dirs -> IO (Either SPPError [BackedUpFile])
createBackups root@(Dirs {srcDir = src, bakDir = bak, outDir = out})
        | src == bak = put src out OutputExistsError
        | src == out = put src bak BackupExistsError
        | otherwise = do
            outResult <- put src out OutputExistsError
            bakResult <- put src bak BackupExistsError
            return . fmap head . sequence $ [outResult, bakResult]
    where
    put :: FilePath -> FilePath -> ErrorType -> IO (Either SPPError [BackedUpFile])
    put from to err = do
        outputExists <- fileOrDirectoryExists to
        if outputExists then
            return . Left $ SPPError err to Nothing Nothing
        else do
            backedUpFiles <- getBkups
            copySuccess <- copyDirectories (sppError BackupError from Nothing) from to
            return $ fmap (const backedUpFiles) copySuccess
    getBkups :: IO [BackedUpFile]
    getBkups = do
        files <- allFiles src
        return $ map (makeBackup root) files


fileOrDirectoryExists :: FilePath -> IO Bool
fileOrDirectoryExists f = liftM2 (||) (doesFileExist f) (doesDirectoryExist f)

{-
Returns whether or not the backup was successfully removed
-}
removeBackups :: RestoreSituation -> Dirs -> IO (Either SPPError ())
removeBackups situation root = do
        backupExists <- doesDirectoryExist buRoot
        if not backupExists then
            return . Left $ SPPError (RestoreError situation) buRoot Nothing Nothing
        else
            dorestore `catch` eitherHandler (sppError (RestoreError situation) buRoot Nothing)
    where
    buRoot = bakDir root
    dorestore
        | srcDir root == bakDir root
            -- if the source directory is the same as the backup directory.
                -- you only need to delete the outputs
            = do
                removeDirectoryRecursive (outDir root)
                return $ Right ()
        | srcDir root == outDir root
            -- if the source is the output, then you need to delete the srcDir
                -- and copy the backup to the source
            = do
                removeDirectoryRecursive (srcDir root)
                renameDirectory buRoot (srcDir root)
                return $ Right ()
        | otherwise
            = do
                removeDirectoryRecursive (outDir root)
                removeDirectoryRecursive (bakDir root)
                return $ Right ()


makeBackup :: Dirs -> FilePath -> BackedUpFile
makeBackup root file = BackedUpFile {
            originalFile = file,
            backupFile = bakDir root </> relative,
            outputFile = outDir root </> relative
        }
    where
    relative = makeRelative (srcDir root) file
