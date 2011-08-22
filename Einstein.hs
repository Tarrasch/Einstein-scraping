module Einstein where

import Network.HTTP
import Text.HTML.TagSoup
import Data.List.Split (splitEvery)
import Control.Monad (guard)

-- I use tag soup as it apperently fixes unicode characters

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

scrapEinstein :: IO (Maybe [[String]])
scrapEinstein = do
    body <- openURL "http://www.butlercatering.se/einstein.html"
    return (parse body)
                   
printMenuToStdout :: IO()
printMenuToStdout = do
    Just sss <- scrapEinstein 
    mapM_ putStrLn $ map unlines sss

parse :: String -> Maybe [[String]]
parse body = guard (map length splitted == [3, 3, 3, 3, 3]) >> Just splitted
  where
    tags = parseTags body
    goodContents = filter (elem '•') [ s | TagText s <- tags ]
    cleanContents = map (dropWhile (`elem` [' ', '•', '\n', '\r'])) goodContents
    splitted = splitEvery 3  cleanContents

