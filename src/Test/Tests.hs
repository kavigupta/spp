{-# LANGUAGE DoAndIfThenElse #-}
module Test.Tests(tests) where

import Test.CmdTests

import Distribution.TestSuite

cmdTests :: [CmdTest]
cmdTests =
        [
          CmdTest "basic"           1 "--src src" "--src src --clean"
        , CmdTest "basic"           2 "--src src --out out" "--src src --out out --clean"
        , CmdTest "tree"            1 "--src src" "--src src --clean"
        , CmdTest "error"           1 "--src src" "--src src --clean"
        , CmdTest "error"           2 "--src src --no-clean-on-errors" "--src src --clean"
        , CmdTest "dirst_simp"      1 "--src src --directive-start '#' --no-clean-on-errors" "--src src --clean"
        , CmdTest "dirst_ignore"    1 "--src src --directive-start '#' --no-clean-on-errors" "--src src --clean"
        , CmdTest "replace"         1 "--src src" "--src src --clean"
        , CmdTest "pass"            1 "--src src" "--src src --clean"
        , CmdTest "passcf"          1 "--src src" "--src src --clean"
        , CmdTest "passse"          1 "--src src" "--src src --clean"
        , CmdTest "exec"            1 "--src src" "--src src --clean"
        , CmdTest "exec"            2 "--src src --out out" "--src src --out out --clean"
        , CmdTest "cwd_curfile"     1 "--src src " "--src src --clean"
        , CmdTest "basicwrite"      1 "--src src " "--src src --clean"
    ]

tests :: IO [Test]
tests = return $ runTests cmdTests
