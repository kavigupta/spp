module Tools.String(
        indentHangs
    ) where

import Data.String.Utils
import Data.Maybe

tabwidth :: Int
tabwidth = 4

tab :: String
tab = replicate tabwidth ' '

chunk :: Int -> [a] -> [[a]]
chunk n list
    | null list         = []
    | length list <= n  = [list]
    | otherwise
        = let (first, rest) = splitAt n list
            in first : chunk n rest

indentHangs :: Int -> String -> String
indentHangs width = concatMap (indentHangsInLine width) . lines . replace "\t" tab

indentHangsInLine :: Int -> String -> String
indentHangsInLine width str = fromMaybe str $ do
        (firstLine, followingLines) <- chunkContent (width - length spaces) content
        let restcs = map (tabspaces ++) followingLines
        return . unlines $ (spaces ++ firstLine):restcs
    where
    tabspaces = tab ++ spaces
    (spaces, content) = span (== ' ') str

chunkContent :: Int -> String -> Maybe (String, [String])
chunkContent widthWospaces content
        | widthWospaces < tabwidth = Nothing
        | otherwise               = Just (firstLine, followingLines)
    where
    (firstLine, followingText) = splitAt widthWospaces content
    followingLines = chunk (widthWospaces - tabwidth) followingText
