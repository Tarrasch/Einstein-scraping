module Parsing where

import Text.HTML.TagSoup
import Data.Char
import Data.List
import Data.List.Split
import Control.Monad


-- | Given site body, return either Nothing or 5*3 array of dishes
parse :: String -> Maybe [[String]]
parse body = guard (map length splitted == [3, 3, 3, 3, 3]) >> Just splitted
  where
    tags = parseTags body
    goodContents = filter (elem '•') [ s | TagText s <- tags ]
    cleanContents = map (dropWhile (`elem` [' ', '•', '\n', '\r'])) goodContents
    splitted = splitEvery 3  cleanContents


-- putStrLn $ unlines $ map (dropWhile (`elem` [' ', '•', '\n', '\r'])) $ filter (elem '•') [ s | TagText s <- tags ]
