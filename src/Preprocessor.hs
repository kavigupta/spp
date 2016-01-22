#!/usr/bin/runhaskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Preprocessor (preprocess) where

import Control.Applicative hiding ((<|>))

import FileHandler
import Interface.Args
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
            -- Ignore the possibility of error at this line.
            contents <- readFile $ backupFile buFile
            -- There should be no error at this line given that process should throw no error
            output <- process sppopts srcPath contents
            case output of
                Left err -> return $ Left err
                Right outputValue -> Right <$> writeFile (outputFile buFile) outputValue
    where srcPath = case sourceLocated buFile of
            AtBak -> backupFile buFile
            AtOut -> outputFile buFile

performCommands :: String -> Directives Command -> IO (Either SPPError String)
performCommands path (Directives header commands rest) = do
        result <- performAll actions rest
        return $ (header ++) <$> result
    where actions = map (getCommand path) commands

{-
Creates an IO instance for preprocessing a series of lines.
-}
process :: SPPOpts -> FilePath -> String -> IO (Either SPPError String)
process sppopts path str
        = either (return . Left) id result
    where
    result :: Either SPPError (IO (Either SPPError String))
    result = performCommands path <$> parseDirectives (directiveStart sppopts) path str
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
