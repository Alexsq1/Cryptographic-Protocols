module RSA where

import Data.ShortWord
import Base64

powMod :: Integer -> Integer -> Integer -> Integer
powMod base exponent modulus = go base exponent 1
  where
    go _ 0 result = result
    go b e result
      | odd e     = go ((b * b) `mod` modulus)
                         (e `div` 2)
                         ((result * b) `mod` modulus)
      | otherwise = go ((b * b) `mod` modulus)
                         (e `div` 2)
                         result

encrypt :: Integer -> Integer -> Integer -> Integer
encrypt mess pKey n = powMod mess pKey n

decrypt :: Integer -> Integer -> Integer -> Integer
decrypt ciph sKey n = powMod ciph sKey n
