module DHTest(dhTest) where

import Test.QuickCheck
import DiffieHellman

dhTest :: IO ()
dhTest = do 
          putStrLn "\nTesting Diffie-Hellman:"
	  quickCheck (dhCorrectness)

-- | Diffie-Hellman Correctness: Same key in both parts
dhCorrectness :: Int -> Int -> Int -> Int -> Property
dhCorrectness a b c d = x === y
    where
    	(x, y) = keyGen (a, b, c, d)

