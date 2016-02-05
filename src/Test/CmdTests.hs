{-# LANGUAGE DoAndIfThenElse #-}
module Test.CmdTests(CmdTest(..), runTests) where

import Prelude hiding (readFile)

import Data.List(sort, intercalate)
import Data.Monoid
import Control.Monad
import Control.Applicative

import System.Directory
import System.Posix.Directory
import System.FilePath
import System.Process
import Data.ByteString(readFile)

import Tools.Files

import Distribution.TestSuite

data CmdTestResult = Success | Failure [String]
    deriving Eq

failure :: String -> CmdTestResult
failure = Failure . return

data CmdTest = CmdTest {
    testName :: String,
    testNumber :: Int,
    testCommandRun :: String,
    testCommandClean :: String
} deriving Show

instance Monoid CmdTestResult where
    mempty = Success
    Success `mappend` x   = x
    x `mappend` Success   = x
    Failure x `mappend` Failure y = Failure (x ++ y)


runTests :: [CmdTest] -> [Test]
runTests = map suiteify

suiteify :: CmdTest -> Test
suiteify test = Test testinst
    where
    testinst :: TestInstance
    testinst = TestInstance {
            run = toProgress <$> runTest test,
            name = show test,
            tags = [],
            options = [],
            setOption = \_ _ -> Right testinst
        }

toProgress :: CmdTestResult -> Progress
toProgress Success = Finished Pass
toProgress (Failure x) = Finished (Fail (intercalate "\n" x))

runTest :: CmdTest -> IO CmdTestResult
runTest CmdTest {testName=tname, testNumber=num, testCommandRun=toRun, testCommandClean=clean} = do
        -- save backup
        _ <- system $ "cp -r " ++ pathTest ++ "/. " ++ pathBak
        startingdir <- getWorkingDirectory
        print startingdir
        let spploc = startingdir </> "spp "
        changeWorkingDirectory pathTest
        getWorkingDirectory >>= print
        -- run spp
        _ <- system $ spploc ++ toRun
        changeWorkingDirectory startingdir
        _ <- system $ "cp -r " ++ pathTest ++ " " ++ pathActual
        changeWorkingDirectory pathTest
        -- check that the result is the same as the desired result
        sppworked <- checkSame "." $ ".." </> resultName
        -- run spp --clean
        _ <- system $ spploc ++ clean
        -- check that the resutl is the same as the original
        cleanworked <- checkSame "." $ "../../testdump" </> backupName
        changeWorkingDirectory startingdir
        removeDirectoryRecursive pathTest
        _ <- system $ "cp -r " ++ pathBak ++ "/. " ++ pathTest
        let worked = sppworked `mappend` cleanworked
        return worked
    where
    resultName = tname ++ "_result" ++ show num
    pathActual = "testdump" </> resultName ++ "__actual"
    backupName = tname ++ "___backup"
    pathTest = "testsuite" </> tname ++ "_test"
    pathBak = "testdump" </> backupName

checkSame :: FilePath -> FilePath -> IO CmdTestResult
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

checkDirsSame :: FilePath -> FilePath -> IO CmdTestResult
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

checkFilesSame :: FilePath -> FilePath -> IO CmdTestResult
checkFilesSame a b = do
    areSame <- liftM2 (==) (readFile a) (readFile b)
    if areSame then
        return Success
    else
        return . failure $ a ++ " and " ++ b ++ " have different contents"
