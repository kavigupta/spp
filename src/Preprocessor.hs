#!/usr/bin/runhaskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Preprocessor (preprocessAll) where

import Control.Exception(catch, evaluate)

import Data.List(isInfixOf)
import FileHandler
import Interface.Args
import CommandGenerator
import Directive.Identifier
import Directive.Parser
import Interface.Errors

preprocessAll :: SPPOpts -> [BackedUpFile] -> IO [Either SPPError ()]
preprocessAll _ [] = return []
preprocessAll opts (x:xs) = 
    do
        c <- preprocess opts ToPreprocess {current=x,future=xs, depChain=[]}
        rest <- preprocessAll opts xs
        return $ c:rest

{-
An IO Action for either processing a file or producing an error without doing anything
-}
preprocess :: SPPOpts -> ToPreprocess -> IO (Either SPPError ())
preprocess sppopts buFile =
        do
            output sppopts Verbose $ "Output File = " ++ outputFile (current buFile) ++ "\n"
            -- Ignore the possibility of error at this line.
            mcontents <- fmap Right (readFile (backupFile . current $ buFile) >>= evaluate)
                    `catch` (return . Left :: IOError -> IO (Either IOError String))
            output sppopts Debug $ "Received output = "++ show mcontents ++ "\n"
            case mcontents of
                (Left err)
                    | isBinaryError err
                        -> return $ Right ()
                    | otherwise
                        -> return $ Left $ sppError ReadIOError (sourceFile . current $ buFile) Nothing err
                (Right contents) -> do
                    -- There should be no error at this line given that process should throw no error
                    outp <- process sppopts buFile contents
                    case outp of
                        SPPFailure err -> return $ Left err
                        SPPSuccess outputValue -> Right <$> writeFile (outputFile . current $ buFile) (fContents outputValue)

isBinaryError :: IOError -> Bool
isBinaryError err = "hGetContents: invalid argument (invalid byte sequence)" `isInfixOf` show err

performCommands :: SPPOpts -> ToPreprocess -> Directives Command -> IO PreprocessorResult
performCommands opts buf (Directives header commands rest) = do
        output opts Debug $ "Directives =" ++ show (Directives header commands rest) ++ "\n"
        result <- performAll actions $ SPPState {cFile=current buf, fContents=rest, dependencyChain=depChain buf, possibleFiles=future buf}
        output opts Debug $ "Result = " ++ show result ++ "\n"
        return $ mapOverSuccess (mapOverContents (header ++)) result
    where actions = map (getCommand (preprocess opts) $ current buf) commands

{-
Creates an IO instance for preprocessing a series of lines.
-}
process :: SPPOpts -> ToPreprocess -> String -> IO PreprocessorResult
process sppopts buf str
        = either (return . SPPFailure) id result
    where
    result :: Either SPPError (IO PreprocessorResult)
    result = performCommands sppopts buf <$> parseDirectives (directiveStart sppopts) (sourceFile . current $ buf) str
{-
    Perform all the actions in the given list of actions.
    If any of the values are `Left` errors, the entire result is an error.
    Otherwise, the result is considered to be all the other actions chained together
-}
performAll :: [Action] -> Action
performAll = foldr comp (return . SPPSuccess)
    where
    comp :: Action -> Action -> Action
    comp g f x = g x >>= \u -> case u of
        (SPPSuccess y) -> f y
        (SPPFailure z) -> return $ SPPFailure z
