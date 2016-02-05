{-# LANGUAGE DoAndIfThenElse #-}
module Test.Tests(tests) where

import Test.CmdTests

import Distribution.TestSuite

cmdTests :: [CmdTest]
cmdTests = [
        CmdTest "basic" 1 "--src src" "--src src --clean",
        CmdTest "basic" 2 "--src src --out out" "--src src --out out --clean",
        CmdTest "tree" 1 "--src src" "--src src --clean",
        CmdTest "error" 1 "--src src" "--src src --clean",
        CmdTest "error" 2 "--src src --no-clean-on-errors" "--src src --clean"
    ]

tests :: IO [Test]
tests = return $ runTests cmdTests
