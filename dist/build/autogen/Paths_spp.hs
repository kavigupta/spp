module Paths_spp (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/kavi/.cabal/bin"
libdir     = "/home/kavi/.cabal/lib/spp-0.1.0.0/ghc-7.6.3"
datadir    = "/home/kavi/.cabal/share/spp-0.1.0.0"
libexecdir = "/home/kavi/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "spp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "spp_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "spp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spp_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
