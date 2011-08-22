module Einstein where

import Network.HTTP
import Parsing

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

