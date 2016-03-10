#!/usr/bin/runhaskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Preprocessor (preprocess) where

import Control.Applicative hiding ((<|>))

import Control.Exception(catch, evaluate)

import FileHandler
import Interface.Args
import Tools.Files
import CommandGenerator
import Directive.Identifier
import Directive.Parser
import Interface.Errors

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
                        Left err -> return $ Left err
                        Right outputValue -> Right <$> writeFile (outputFile buFile) outputValue

performCommands :: SPPOpts -> BackedUpFile -> Directives Command -> IO PreprocessorResult
performCommands opts buf (Directives header commands rest) = do
        output opts Debug $ "Directives =" ++ show (Directives header commands rest) ++ "\n"
        result <- performAll actions rest
        output opts Debug $ "Result = " ++ show result ++ "\n"
        return $ (header ++) <$> result
    where actions = map (getCommand buf) commands

{-
Creates an IO instance for preprocessing a series of lines.
-}
process :: SPPOpts -> BackedUpFile -> String -> IO (Either SPPError String)
process sppopts buf str
        = either (return . Left) id result
    where
    result :: Either SPPError (IO (Either SPPError String))
    result = performCommands sppopts buf <$> parseDirectives (directiveStart sppopts) (sourceFile buf) str
{-
    Perform all the actions in the given list of actions.
    If any of the values are `Left` errors, the entire result is an error.
    Otherwise, the result is considered to be all the other actions chained together
-}
performAll :: (Monad m) => forall a err. [a -> m (Either err a)] -> a -> m (Either err a)
performAll = foldr comp (return . return)
    where
    comp f g x = bind2 (f x) g
    bind2 val f = do
        x <- val
        either (return . Left) f x
