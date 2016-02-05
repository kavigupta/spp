{-# LANGUAGE DoAndIfThenElse #-}
module Test.Tests(tests) where

import Test.CmdTests

import Distribution.TestSuite

cmdTests :: [CmdTest]
cmdTests = [
        CmdTest "basic" 1 "--src src",
        CmdTest "basic" 2 "--src src --out out",
        CmdTest "tree" 1 "--src src",
        CmdTest "error" 1 "--src src"
    ]

tests :: IO [Test]
tests = return $ runTests cmdTests
