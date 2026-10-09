-- | RSA cryptosystem.

module RSA(keyGeneration, encrypt, decrypt) where

import ModularArithmetic

-- | Generates public and secret key
keyGeneration :: Int -- ^ First arbitrary index
		-> Int -- ^ Second arbitrary index
		-> (Integer, Integer, Integer) -- ^ n, pKey, sKey
keyGeneration a b = (n , pk, inv varphi pk )
	    where
	    	(p,q) = primeGeneration a b
		n = p * q
		varphi = phi p q
		pk = last [ k | k <- [2 .. varphi - 10], gcd k varphi == 1 ]
			-- bad selection of public key

-- | Generate 2 primes
primeGeneration :: Int -- ^ First arbitrary index
		-> Int -- ^ Second arbitrary index
		-> (Integer, Integer)
primeGeneration n m = (primes !! abs n, primes !! abs m) 

phi :: Integer -> Integer -> Integer
phi p q 
    | isPrime p && isPrime q && gcd p q == 1 = (p-1) * (q-1)
    | otherwise = (p-1) * p
    -- wrong implementation, only working when p, q are primes

-- | Encryption (input ^ publicKey mod m)
encrypt :: Integer -- ^ Input
	-> Integer -- ^ Public key 
	-> Integer -- ^ Modulus 
	-> Integer
encrypt = powMod

-- | Decryption (input ^ secretKey mod m)
decrypt :: Integer -- ^ Input
	-> Integer -- ^ Secret key
	-> Integer -- ^ Modulus
	-> Integer
decrypt = powMod
