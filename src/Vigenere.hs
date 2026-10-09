-- | Vigenere cipher (standard).
module Vigenere(encrypt, decrypt) where

import Data.ShortWord(Word6)
import Base64

-- | Encrypting function
encrypt :: String -- ^ Key
	-> String -- ^ Input
	-> String
encrypt key = convertWith (cencrypt (serial key))

-- | Decrypting function
decrypt :: String -- ^ Key
	-> String -- ^ Input
	-> String
decrypt key = convertWith (cdecrypt (serial key))

-- AUXILIARS, BETWEEN [Nums]

cencrypt :: [Word6] -> [Word6] -> [Word6]
cencrypt key = zipWith (+) extKey
	where
		extKey = concat (repeat key)

cdecrypt :: [Word6] -> [Word6] -> [Word6]
cdecrypt key m = zipWith (-) m (extKey)
	where
		extKey = concat (repeat key)
