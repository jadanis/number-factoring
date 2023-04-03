import UserInteract (getBool, getInt)
import IntFactor (showFactor)

main :: IO ()
main = do
  factorInt
  continue

continue :: IO ()
continue = do
  putStrLn "Factor another number?"
  b <- getBool
  if b then main else end

end :: IO ()
end = return ()

factorInt :: IO ()
factorInt = do
  n <- getInt
  putStrLn $ showFactor n
