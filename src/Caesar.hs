-- | Caesar cipher, encrypting and decrypting functions.

module Caesar(encrypt, decrypt) where

import Data.ShortWord(Word6)
import Base64

-- | Encrypting function
encrypt :: Word6 -- ^ Key
	-> String -- ^ Input
	-> String
encrypt key = convertWith (cencrypt key)

-- | Decrypting function
decrypt :: Word6 -- ^ Key
	-> String -- ^ Input
	-> String
decrypt key = convertWith (cdecrypt key)

-- AUXILIARS, BETWEEN [Nums]

cencrypt :: Word6 -> [Word6] -> [Word6]
cencrypt key = map (+ key)

cdecrypt :: Word6 -> [Word6] -> [Word6]
cdecrypt key = map (\x -> x - key)
