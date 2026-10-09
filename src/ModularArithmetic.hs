-- | Modular Arithmetic basic operations.

module ModularArithmetic(powMod, inv, primes, isPrime) where

import Prelude hiding (gcd)

-- | Efficient power in modulus. O(log e)
powMod :: Integer  -- ^ Base
	-> Integer -- ^ Exponent
	-> Integer -- ^ Modulus
	-> Integer
powMod base ex m = go 1 base ex
  where
    go acc _ 0 = acc
    go acc b e 

	-- Odd: b ^ (2k + 1) = b * b ^ (2k) = b * (b ^ 2) ^ k
	
      | odd e     = go ((acc * b) `mod` m)
		       ((b * b) `mod` m)
		       (e `div` 2)

	-- Even: b ^ (2k) = (b ^ 2) ^ k
      | otherwise = go acc
		         ((b * b) `mod` m)
                         (e `div` 2)

-- | Greatest common divisor by euclidean algorithm
--gcd :: Integer -> Integer -> Integer
--gcd a 0 = a
--gcd a b = gcd b r
--	where
--		r = a `mod` b


-- | Calculates a particular solution (g, x, y) 
-- of the diophantic equation a*x + b*y = g, where g = gcd(a,b).
--
diophantic :: Integer -- ^ a
	-> Integer -- ^ b
	-> (Integer, Integer, Integer)
diophantic a 0 = (a, 1, 0)
diophantic a b =
  let (g, x', y') = diophantic b (a `mod` b)
  in  (g, y', x' - (a `div` b) * y')

-- the computation of g is identical to gcd by euclidean alg.
-- Proof of correctness:
-- Base case, b = 0:
-- g = a, x = 1, y = 0.
-- a * 1 + b * 0 = a
--
-- Induction case, b /= 0
-- Recall r = a % b
-- q = a / b
-- a = b q + r
-- By IH, g = b x' + r y'. 
-- We are returning 
-- x := y'
-- y := x' - q y'
-- (Note that x' = y + q y' = y + q x)
--
-- This is correct because:
-- g = b x' + r y' = b (y + q x) + r x = 
-- b q x + b y + r x = (b q + r) x + b y = a x + b y = q
--
-- (Perhaps is not a proper proof, but gives us an idea of the values)
--


-- | Calculates the inverse in a modulus, if exists. Finds x s. t. a * x = 1 (mod m)
inv :: Integer -- ^ m (modulus)
	-> Integer -- ^ a
	-> Integer
inv m a =
  case diophantic a m of
    (1, x, _) -> (x `mod` m)
    _         -> error ( (show a) 
    			++ " does not have an inverse in modulus " ++ (show m))

-- | Lazy list of primes
primes :: [Integer]
primes = sieve [2..] where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- | Primality test, O(sqrt n)
isPrime :: Integer -> Bool
isPrime = primality primes
    where
    	primality :: [Integer] -> Integer -> Bool
	primality (p:ps) n
	    | p > floor (sqrt (fromIntegral n)) = True
	    | otherwise = (n `mod` p /= 0) && primality ps n

