module RSA(encrypt, decrypt) where

--import Data.ShortWord
import ModularArithmetic

encrypt :: Integer -> Integer -> Integer -> Integer
encrypt mess pKey n = powMod mess pKey n

decrypt :: Integer -> Integer -> Integer -> Integer
decrypt ciph sKey n = powMod ciph sKey n
