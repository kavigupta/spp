{-# LANGUAGE DoAndIfThenElse #-}
module FileHandler(
        BackedUpFile(..), sourceFile, sourceIs,
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

data BackedUpFile = BackedUpFile {
    sourceLocated :: SourceLocation,
    backupFile :: FilePath,
    outputFile :: FilePath
} deriving (Show)

instance Eq BackedUpFile where
    a == b  =
        backupFile a == backupFile b &&
        outputFile a == outputFile b &&
        sourceFile a == sourceFile b

sourceFile :: BackedUpFile -> FilePath
sourceFile buf = case sourceLocated buf of
    AtBak -> backupFile buf
    AtOut -> outputFile buf

sourceIs :: BackedUpFile -> FilePath -> Bool
sourceIs buf path = sourceFile buf == path

copyDirectories :: IOExcHandler -> FilePath -> FilePath -> IO (Either SPPError ())
copyDirectories handler src dst = unsafeCp `catch` eitherHandler handler
    where
    unsafeCp = Right <$> shelly ((cp_r `on` fromText . pack) src dst)

setWritable :: Bool -> FilePath -> IO ()
setWritable b f = do
    p <- getPermissions f
    setPermissions f (p {writable = b})

createBackups :: Dirs -> IO (Either SPPError [BackedUpFile])
createBackups root@Dirs {srcLoc=srcl, bakOf=bak, outOf=out}
        = case srcl of
            AtBak ->
                put bak out OutputExistsError
            AtOut ->
                put out bak BackupExistsError
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
    src = case srcl of
        AtBak -> bak
        AtOut -> out
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
removeBackups situation (Dirs {outOf=out, bakOf=bak, srcLoc=srclc}) = do
        backupExists <- doesDirectoryExist bak
        if not backupExists then
            return . Left $ SPPError (RestoreError situation) bak Nothing Nothing
        else
            dorestore >> return (Right ())
                `catch` eitherHandler (sppError (RestoreError situation) bak Nothing)
    where
    dorestore = case srclc of
        AtBak ->
            -- if the source directory is the same as the backup directory.
                -- you only need to delete the outputs
            removeDirectoryRecursive out
        AtOut ->
            -- if the source is the output, then you need to delete the srcDir
                -- and copy the backup to the source
            do
                removeDirectoryRecursive out
                renameDirectory          bak out


makeBackup :: Dirs -> FilePath -> BackedUpFile
makeBackup (Dirs {outOf=out, bakOf=bak,srcLoc=src}) file = BackedUpFile {
            sourceLocated = src,
            backupFile = bak </> relative,
            outputFile = out </> relative
        }
    where
    srcDir = case src of
        AtOut -> out
        AtBak -> bak
    relative = makeRelative srcDir file
