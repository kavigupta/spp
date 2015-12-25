module Tools.Files(
        eitherHandler
    ) where

import Interface.Errors
import Control.Exception

-- Handles an error by using the left error reporting mechanism
eitherHandler :: IOException -> IO (Either SPPError a)
eitherHandler err = return . Left $ IOError err
