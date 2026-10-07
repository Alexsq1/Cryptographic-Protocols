module Caesar(encrypt, decrypt) where

import Data.ShortWord(Word6)
import Base64

encrypt :: Word6 -> String -> String
encrypt key = convertWith (cencrypt key)

decrypt :: Word6 -> String -> String
decrypt key = convertWith (cdecrypt key)

cencrypt :: Word6 -> [Word6] -> [Word6]
cencrypt key = map (+ key)

cdecrypt :: Word6 -> [Word6] -> [Word6]
cdecrypt key = map (\x -> x- key)
