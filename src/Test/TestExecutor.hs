{-# LANGUAGE DoAndIfThenElse #-}
module Test.TestExecutor(Test(..), TestResult(..), runTests, runTest) where

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

data TestResult = Success | Failure [String]
    deriving Eq

failure :: String -> TestResult
failure = Failure . return

data Test = Test {
    testName :: String,
    testNumber :: Int,
    testCommand :: String
}

instance Monoid TestResult where
    mempty = Success
    Success `mappend` x   = x
    x `mappend` Success   = x
    Failure x `mappend` Failure y = Failure (x ++ y)


runTests :: [Test] -> IO TestResult
runTests tests = do
    results <- forM tests runTest
    let ntrials = length tests
    let nsuccess = length $ filter (==Success) results
    let result = mconcat results
    case result of
        Success -> putStrLn $ show ntrials ++ " tests suceeded!"
        Failure fs -> putStrLn $ show nsuccess ++ "/" ++ show ntrials ++ " succeeded. Rest failed."
    putStrLn "asdf"
    return result

runTest :: Test -> IO TestResult
runTest Test {testName=testName, testNumber=num, testCommand=cmd} = do
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
        changeWorkingDirectory startingdir
        removeDirectoryRecursive pathTest
        system $ "cp -r " ++ pathBak ++ "/. " ++ pathTest
        let worked = sppworked `mappend` cleanworked
        return worked
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
            return $ failure $ a ++ " and " ++ b ++ " are not the same file-directory type thingy"

checkDirsSame :: FilePath -> FilePath -> IO TestResult
checkDirsSame a b = do
    conta <- sort <$> realContents a
    contb <- sort <$> realContents b
    if conta /= contb then
        return . failure $
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
        return . failure $ a ++ " and " ++ b ++ " have different contents"
