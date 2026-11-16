module ModArithmTest(maTest) where

import Test.QuickCheck
import Data.List((\\))
import ModularArithmetic

maTest :: IO ()
maTest = do 
          putStrLn "\nModular Arithmetic"
	  quickCheck (testPowModPos)
	  quickCheck (testPowModZ)
	  quickCheck (testPowModNeg)
	  --quickCheck (testDiophantic)
	  quickCheck (testInv)
	  quickCheck (testPrimeCongr)
	  quickCheck (testIsPrime)

-- | powMod works as without optimizations
testPowModPos :: Integer -> Integer -> Integer -> Property
testPowModPos base ex m = m > 0 && ex >= 0 ==> powMod base ex m === (base ^ ex) `mod` m

-- | powMod with exponent zero
testPowModZ :: Integer -> Integer -> Property
testPowModZ base m = m > 1 ==> powMod base 0 m === 1

-- | powMod is congruent with negative numbers and inverses: b ^ -e * b ^ e = b ^ 0 = 1
testPowModNeg :: Integer -> Integer -> Integer -> Property
testPowModNeg base ex m = m > 0 && ex < 0 && gcd nb m == 1 ==> (powMod nb ex m * powMod nb (abs ex) m) `mod` m === (1 `mod` m)
    where
    	-- new base: the positive equivalent number
        nb = mod base m

-- | The diophantic equation solution is correct: ax + by = g, and g is gcd
--testDiophantic :: Integer -> Integer -> Property
--testDiophantic a b = property (g == gcd a b && a * x + b * y == g)
----testDiophantic a b = property ((a == 0 && b == 0 && g == 0) || (g > 0 && mod a g == 0 && mod b g == 0 && a * x + b * y == g))
--    where
--        (g, x, y) = diophantic a b

-- | Inverse is correct: a ^ -1 * a = 1
testInv :: Integer -> Integer -> Property
testInv x m = m > 0 && gcd nx m == 1 ==> (nx * invx) `mod` m === (1 `mod` m)
    where
    	-- new base: the positive equivalent number
        nx = mod x m
	invx = inv m nx

-- | All primes satisfy the primality
testPrimeCongr :: Int -> Property
testPrimeCongr n = n > 0 ==> isPrime (primes !! n)

-- | All primes satisfy the primality
testIsPrime :: Integer -> Property
testIsPrime n = n > 1 ==> property (if isPrime n then (elem n primes) else (last (takeWhile (\x -> x <= n) primes) /= n))

