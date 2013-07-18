module Glob (glob, matches) where

import Text.Regex.Posix (match, makeRegexOpts, defaultCompOpt,
                         compIgnoreCase, defaultExecOpt)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>), takeDirectory, takeFileName)
import Control.Monad (filterM)

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = "." ++ globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = "[" ++ c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
  where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs) = c : globToRegex' cs
charClass [] = error "unterminated character class"

matches :: String -> String -> Bool
matches name pat = regex `match` name
  where regex = makeRegexOpts (defaultCompOpt + compIgnoreCase)
                              defaultExecOpt
                              (globToRegex pat)

glob :: String -> IO [FilePath]
glob pat | isRoot dir = listFiles dir name
         | otherwise = glob dir >>= filterM doesDirectoryExist >>=
                       mapM (`listFiles` name) >>= return . concat
  where dir = takeDirectory pat
        name = takeFileName pat

isRoot :: FilePath -> Bool
isRoot path = takeDirectory path == "." || takeDirectory path == path

listFiles :: FilePath -> String -> IO [FilePath]
listFiles dir pat = getDirectoryContents dir >>= return .
    map (dir </>) .
    filter (\name -> name `notElem` [".", ".."] && name `matches` pat)
