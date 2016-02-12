#!/usr/bin/runhaskell

import System.Process
import System.Directory
import Network.Download
import System.Posix.Directory
import System.Exit
import System.FilePath.Glob
import Control.Exception
import qualified Data.ByteString as B

import Data.List
import Control.Applicative

import Text.Regex
import Control.Monad

targz, folder :: FilePath
targz = folder ++ ".tar.gz"
folder = "process-1.4.2.0"

main :: IO ()
main = do
    download
    system $ "tar xzf " ++ targz
    changeWorkingDirectory folder
    dat <- readFile "process.cabal"
    writeFile "processnew.cabal" (rename dat)
    removeFile "process.cabal"
    hsc <- glob "**/*.hsc"
    hs <- glob "**/*.hs"
    let glo = hs ++ hsc
    let haskells = filter isProcExc glo 
    print haskells
    forM haskells dofix
    system $ "mv System/Process System/ProcessNew"
    renameFile "System/Cmd.hs" "System/CmdNew.hs"
    renameFile "System/Process.hsc" "System/ProcessNew.hsc"
    system $ "runhaskell Setup configure"
    system $ "runhaskell Setup build"
    system $ "runhaskell Setup install"
    changeWorkingDirectory ".."
    exitFailure
    removeDirectoryRecursive folder
    removeFile targz
    return ()

isProcExc :: String -> Bool
isProcExc path = "Process" `isInfixOf` path || "Cmd" `isInfixOf` path

dofix :: FilePath -> IO ()
dofix path = do
        text <- readFile path >>= evaluate
        writeFile (fixPath path) (fixPkg text)
        removeFile path

fixPath :: FilePath -> FilePath
fixPath path = case index of
        Just val -> let (first, ext) = splitAt val path in
            first ++ "New" ++ ext
        Nothing -> path
    where
    index = ((length path-1)-) <$> findIndex (=='.') (reverse path)

fixPkg :: String -> String
fixPkg = modPr "Process" . modPr "Cmd"

modPr :: String -> String -> String
modPr pkg output = subRegex
    (mkRegex $
        "System\\." ++ pkg)
    output
    ("System." ++ pkg ++ "New")

download :: IO ()
download = do
    doc <- openURI "http://hackage.haskell.org/package/process-1.4.2.0/process-1.4.2.0.tar.gz"
    case doc of
        Left lef -> error lef
        Right dc -> B.writeFile targz dc

rename :: String -> String
rename str = initial ++ spaces ++ "processnew\n" ++ fixPkg rest'' 
    where
    (initial, rest) = span (/=' ') str
    (spaces, rest') = span (==' ') rest
    (_, rest'') = span (/='\n') rest'
