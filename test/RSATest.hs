module RSATest(rsaTest) where

import Test.QuickCheck
import Data.ShortWord(Word6)
import Base64Test
import RSA

rsaTest :: IO ()
rsaTest = do 
          putStrLn "\nTesting RSA:"
	  quickCheck (rsaCorrectness)
	  --quickCheck (unsafeKey)

-- | RSA Correctness: 
rsaCorrectness :: Int -> Int -> Integer -> Property
rsaCorrectness s t m = (s >= 2 && t >= 2 && m >= 0 && (s /= t || gcd m n == 1)) ==> 
				(decrypt (encrypt m pk n) sk n) === m
    where
    	(n, pk, sk) = keyGeneration s t

-- the cipher correctness fails when the primes are equal
-- and the message is not coprime with the modulus

-- | key 1 is unsafe
unsafeKey :: Integer -> Integer -> Property
unsafeKey m n = (m >= 0 && n >= 0 && m < n) ==> (encrypt m 1 n) === m
