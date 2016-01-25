{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Test.TestExecutor

import System.Exit (exitFailure)

import Control.Monad
import Data.List

tests :: [Test]
tests = [
        Test "basic" 1 "--src src",
        Test "basic" 2 "--src src --out out"
    ]

main :: IO ()
main = do
    result <- runTests tests
    case result of
        Failure str -> do
            putStrLn "Failed Tests!"
            putStrLn (intercalate "\n" str)
            exitFailure
        Success -> return ()
