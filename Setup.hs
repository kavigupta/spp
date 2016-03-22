#!/usr/bin/runhaskell
import Distribution.Simple

import Data.List

import System.Process
import System.Exit
import System.Directory
import System.Environment

import Control.Monad

import Text.Regex.Posix((=~))

testDumpLocation, testSuiteLocation :: FilePath
testDumpLocation = "/mnt/testdump"
testSuiteLocation = "/mnt/testsuite"

main = do
    args <- getArgs
    case args of
        [] -> build
        ["mount"] -> doMounts
        ["unmount"] -> doUnmounts
        _ -> die $ "Invalid arguments " ++ unwords args

build :: IO ()
build = do
    configureCode <- rawSystem "cabal" ["configure", "--enable-tests"]
    buildCode <- rawSystem "cabal" ["build"]
    when (buildCode /= ExitSuccess) $ die "Build failed"
    hlintRun
    copyFile "dist/build/spp/spp" "spp"
    exists <- doesFileExist "log"
    when exists $ removeFile "log"
    doMounts
    buildTest <- rawSystem "cabal" ["test"]
    when (buildTest /= ExitSuccess) $ die "Tests failed"
    doUnmounts

hlintRun :: IO ()
hlintRun = do
    (_,lintResult1, lintResult2) <- readProcessWithExitCode "hlint" ["src"] ""
    let lintResult = lintResult1 ++ lintResult2
    when (lintResult =~ "([0-9]+)\\s+suggestions?")
        $ die ("Hlint suggestions ==>\n" ++ lintResult)

die :: String -> IO ()
die msg = putStrLn msg >> exitFailure

doMounts :: IO ()
doMounts = do
    _ <- system "rm testdump-file"
    _ <- system "mkfs -t ext4 testdump-file 10000"
    _ <- system $ "sudo mkdir -p " ++ testDumpLocation
    _ <- system $ "sudo mount -t ext4 testdump-file " ++ testDumpLocation
    _ <- system $ "sudo chown $USER " ++ testDumpLocation
    _ <- system $ "sudo mkdir -p " ++ testSuiteLocation
    _ <- system $ "sudo mount -t ext4 testsuite-file " ++ testSuiteLocation
    _ <- system $ "sudo chown $USER " ++ testSuiteLocation
    return ()

doUnmounts :: IO ()
doUnmounts = do
    _ <- system $ "sudo umount -l " ++ testDumpLocation
    _ <- system $ "sudo rmdir " ++ testDumpLocation
    _ <- system $ "sudo umount -l " ++ testSuiteLocation
    _ <- system $ "sudo rmdir " ++ testSuiteLocation
    return ()
