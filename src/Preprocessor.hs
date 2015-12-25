#!/usr/bin/runhaskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Preprocessor (preprocess) where

import Control.Applicative hiding ((<|>))

import FileHandler
import Interface.Args
import CommandGenerator
import Directive.Identifier
import Interface.Errors

{-
An IO Action for either processing a file or producing an error without doing anything
-}
preprocess :: Options -> BackedUpFile -> IO (Either SPPError ())
preprocess opts buFile =
        do
            -- Ignore the possibility of error at this line.
            contents <- readFile $ backupFile buFile
            -- There should be no error at this line given that process should throw no error
            output <- process opts (originalFile buFile) contents
            case output of
                Left err -> return $ Left err
                Right outputValue -> Right <$> writeFile (originalFile buFile) outputValue

{-
Creates an IO instance for preprocessing a series of lines.
-}
process :: Options -> FilePath -> String -> IO (Either SPPError String)
process opts path str
        = case result of
            Left err -> return $ Left err
            Right res -> res
    where
        result :: Either SPPError (IO (Either SPPError String))
        result = do
            (header, commands, rest) <- parseDirectives path (directiveStart opts) str :: Either SPPError (String, [Action], String)
            return $ fmap (header ++) <$> performAll commands rest
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
