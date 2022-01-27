import UserInteract (getBool, getInt)
import IntFactor (showFactor)

main :: IO ()
main = factorInt >> continue

continue :: IO ()
continue = (putStrLn "Factor another number?") >> getBool >>= (\b -> if b then main else end)

end :: IO ()
end = return ()

factorInt :: IO ()
factorInt = getInt >>= (putStrnLn . showFactor)
