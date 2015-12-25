module Tools.Files(
        eitherHandler
    ) where

import Control.Exception

-- Handles an error by using the left error reporting mechanism
eitherHandler :: IOException -> IO (Either String a)
eitherHandler err = return . Left $ "An error occured in preprocessing\n" ++ show err
