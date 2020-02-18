module Util
where

-- Citire conținut fișier --
getInputTest fileName = do  
    contents <- readFile $ "input/" ++ fileName
    return contents
