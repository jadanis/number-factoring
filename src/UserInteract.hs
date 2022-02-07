module UserInteract
( getBool
, getInt
) 
where

import Control.Exception (IOException, catch)
import Data.Char (toLower,toUpper)

getInt :: IO Int
getInt = getTypeLoop getInt' "Please enter a positive integer: "

getInt' :: IO Int
getInt' = do
  n  <- readLn
  case compare n 0 of
    GT -> return n
    _ -> ioError (userError "Must be a positive integer")

getBool :: IO Bool
getBool = getTypeLoop getBool' "Please enter True or False?"

getBool' :: IO Bool
getBool' = do
  s <- getLine
  let mbool = tryBool s
  case mbool of
    Just b -> return b
    Nothing -> ioError (userError "Could not deduce boolean")

tryBool :: String -> Maybe Bool
tryBool s 
  | s' == "True" || s' == "T" || s' == "Y" || s' == "Yes" = Just True
  | s' == "False" || s' == "F" || s' == "N" || s' == "No" = Just False
  | otherwise = Nothing
  where s' = capitalized s

capitalized :: String -> String
capitalized (head:tail) = toUpper head : map toLower tail
capitalized [] = []

getTypeLoop :: Read a => IO a -> String -> IO a
getTypeLoop getter init_msg = catch initGet handler
  where initGet = (putStrLn init_msg) >> getter
        handler' :: IOException -> IO ()
        handler' err = putStrLn $ "Whoops!: " ++ (show err)
        handler err = (handler' err) >> (getTypeLoop getter init_msg)
