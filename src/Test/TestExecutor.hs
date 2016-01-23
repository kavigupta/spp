{-# LANGUAGE DoAndIfThenElse #-}
module Test.TestExecutor where

import Prelude hiding (readFile)

import Data.List(sort)
import Data.Monoid
import Data.Function
import Control.Monad
import Control.Applicative

import Data.Text(pack)

import System.Directory
import System.Posix.Directory
import System.FilePath
import System.Process
import Data.ByteString(readFile)

import Tools.Files

data TestResult = Success | Failure String deriving Eq

instance Monoid TestResult where
    mempty = Success
    Failure x `mappend` _ = Failure x
    Success `mappend` x   = x

runTest :: String -> Int -> String -> IO TestResult
runTest testName num cmd = do
        -- save backup
        system $ "cp -r " ++ pathTest ++ "/. " ++ pathBak
        startingdir <- getWorkingDirectory
        print startingdir
        let spploc = startingdir </> "spp "
        changeWorkingDirectory pathTest
        getWorkingDirectory >>= print
        print $ spploc ++ cmd
        -- run spp
        system $ spploc ++ cmd
        -- check that the result is the same as the desired result
        sppworked <- checkSame "." $ ".." </> resultName
        -- run spp --clean
        system $ spploc ++ cmd ++ " --clean"
        -- check that the resutl is the same as the original
        cleanworked <- checkSame "." $ ".." </> backupName
        if sppworked /= Success then
            return sppworked
        else do
            changeWorkingDirectory startingdir
            return cleanworked
    where
    resultName = testName ++ "_result" ++ show num
    backupName = testName ++ "___backup"
    pathTest = "testsuite" </> testName ++ "_test"
    pathResult = "testsuite" </> resultName
    pathBak = "testsuite" </> backupName

checkSame :: FilePath -> FilePath -> IO TestResult
checkSame a b = do
    adir <- doesDirectoryExist a
    bdir <- doesDirectoryExist b
    if adir && bdir then
        checkDirsSame a b
    else
        if not adir && not bdir then
            checkFilesSame a b
        else
            return $ Failure $ a ++ " and " ++ b ++ " are not the same file-directory type thingy"

checkDirsSame :: FilePath -> FilePath -> IO TestResult
checkDirsSame a b = do
    conta <- sort <$> realContents a
    contb <- sort <$> realContents b
    if conta /= contb then
        return . Failure $
            a ++ " and " ++ b ++ " have different contents; "
                ++ show conta ++ " /= " ++ show contb
    else do
        let conta' = map (a</>) conta
        let contb' = map (b</>) contb
        results <- zipWithM checkSame conta' contb'
        return $ mconcat results

checkFilesSame :: FilePath -> FilePath -> IO TestResult
checkFilesSame a b = do
    areSame <- liftM2 (==) (readFile a) (readFile b)
    if areSame then
        return Success
    else
        return . Failure $ a ++ " and " ++ b ++ " have different contents"
