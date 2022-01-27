module UserInteract
( getBool
, getInt
) 
where

import Control.Exception (IOException, catch)

getInt :: IO Int
getInt = getTypeLoop getInt' "Please enter a positive integer: " "Looks like you did not enter a positive integer. Try again: "

getInt' :: IO Int
getInt' = do
  n :: Int <- readLn
  case compare n 0 of
    GT -> return n
    _ -> ioError (userError "Must be a positive integer")

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
getBool' = readLn


getTypeLoop :: Read a => IO a -> String -> IO a
getTypeLoop getter init_msg = catch initGet handler
  where initGet = (putStrLn init_msg) >> getter
        handler' :: IOException -> IO ()
        handler' err = putStrLn $ "Whoops!: " ++ (show err)
        handler err = (handler' err) >> (getTypeLoop getter init_msg)
