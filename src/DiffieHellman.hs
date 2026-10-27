module DiffieHellman where
import ModularArithmetic(powMod, primeGeneration)

-- | Key generation with Diffie-Hellman algorithm. Return the values of Alice and Bob, and should be the same.
keyGen :: (Int, Int, Int, Int) -- ^ Seeds
        -> (Integer, Integer)

keyGen (s1p, s2p, s3p, s4p) = (powMod (powMod g alpha p) beta p  , (powMod (powMod g beta p) alpha p))
    where
    	-- ensures positive seeds and non-empty selections
	(s1, s2, s3, s4) = (2 + abs s1p, abs s2p, abs s3p, abs s4p)
        (p, _) = primeGeneration s1 s1

	-- Zp* = Zp when p is prime?
	validGs = [ n | n <- [0 .. (p - 1)] , gcd n p == 1 ]
	g = safeAcc s2 validGs

	validExponents = [ 0 .. (p - 2) ]

	-- Alice computation:
	alpha = safeAcc s3 validExponents 

	-- Bob computation:
	beta = safeAcc s4 validExponents 

safeAcc :: Int -> [Integer] -> Integer
safeAcc n xs = xss !! nn
    where
        xss = filter (\x -> x >= 2) xs
	nn = (abs n) `mod` (length xss)

