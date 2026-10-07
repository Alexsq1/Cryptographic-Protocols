module Vigenere where

import Data.ShortWord(Word6)
import Base64

encrypt :: String -> String -> String
encrypt key = convertWith (cencrypt (serial key))

decrypt :: String -> String -> String
decrypt key = convertWith (cdecrypt (serial key))

cencrypt :: [Word6] -> [Word6] -> [Word6]
cencrypt key = zipWith (+) extKey
	where
		extKey = concat (repeat key)

cdecrypt :: [Word6] -> [Word6] -> [Word6]
cdecrypt key m = zipWith (-) m (extKey)
	where
		extKey = concat (repeat key)
