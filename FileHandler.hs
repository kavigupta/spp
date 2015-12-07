module FileHandler(
        allFiles
    ) where

import System.Directory
import Control.Applicative

allFiles :: FilePath -> IO [FilePath]
allFiles = filter (not . isDirectory) <$> allFilesAndDirectories

allFilesAndDirectories :: FilePath -> IO [FilePath]
