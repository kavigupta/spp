#!/usr/bin/runhaskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Preprocessor (preprocessAll) where

import Control.Applicative hiding ((<|>))
import Control.Monad

import Control.Exception(catch, evaluate)

import FileHandler
import Interface.Args
import Tools.Files
import CommandGenerator
import Directive.Identifier
import Directive.Parser
import Interface.Errors

preprocessAll :: SPPOpts -> [BackedUpFile] -> IO [Either SPPError ()]
preprocessAll opts buf = forM buf $ preprocess opts

{-
An IO Action for either processing a file or producing an error without doing anything
-}
preprocess :: SPPOpts -> BackedUpFile -> IO (Either SPPError ())
preprocess sppopts buFile =
        do
            output sppopts Verbose $ "Output File = " ++ outputFile buFile ++ "\n"
            -- Ignore the possibility of error at this line.
            mcontents <- fmap Right (readFile (backupFile buFile) >>= evaluate)
                    `catch` (return . Left . show :: IOError -> IO (Either String String))
            output sppopts Debug $ "Received output = "++ show mcontents ++ "\n"
            case mcontents of
                (Left _) -> return $ Right ()
                (Right contents) -> do
                    -- There should be no error at this line given that process should throw no error
                    outp <- process sppopts buFile contents
                    case outp of
                        SPPFailure err -> return $ Left err
                        SPPSuccess outputValue -> Right <$> writeFile (outputFile buFile) outputValue
                        SPPRequire _ _ -> error "Illegal state spprequire"

performCommands :: SPPOpts -> BackedUpFile -> Directives Command -> IO PreprocessorResult
performCommands opts buf (Directives header commands rest) = do
        output opts Debug $ "Directives =" ++ show (Directives header commands rest) ++ "\n"
        result <- performAll actions rest
        output opts Debug $ "Result = " ++ show result ++ "\n"
        return $ mapOverSuccess (header ++) result
    where actions = map (getCommand buf) commands

{-
Creates an IO instance for preprocessing a series of lines.
-}
process :: SPPOpts -> BackedUpFile -> String -> IO PreprocessorResult
process sppopts buf str
        = either (return . SPPFailure) id result
    where
    result :: Either SPPError (IO PreprocessorResult)
    result = performCommands sppopts buf <$> parseDirectives (directiveStart sppopts) (sourceFile buf) str
{-
    Perform all the actions in the given list of actions.
    If any of the values are `Left` errors, the entire result is an error.
    Otherwise, the result is considered to be all the other actions chained together
-}
performAll :: [String -> IO PreprocessorResult] -> String -> IO PreprocessorResult
performAll = foldr comp (return . SPPSuccess)
    where
    comp g f x = g x >>= \u -> case u of
        (SPPSuccess y) -> f y
        (SPPFailure z) -> return $ SPPFailure z
