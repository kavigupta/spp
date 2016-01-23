{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Test.TestExecutor

import System.Exit (exitFailure)

import Control.Monad

main :: IO ()
main = do
    result <- runTest "basic" 1 "--src src"
    case result of
        Failure str -> error str
        Success -> return ()
