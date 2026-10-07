module Base64(serial, deserial, convertWith) where

import Data.ShortWord(Word6)
import Data.List(elemIndex)

serial :: String -> [Word6]
serial = map charToNum

deserial :: [Word6] -> String 
deserial = map numToChar

alphabet :: String
alphabet = ( ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['+' , '/'] )

numToChar :: Word6 -> Char
numToChar n = alphabet !! (fromIntegral n)

charToNum :: Char -> Word6
charToNum c = 
            case (elemIndex c alphabet) of
	    	Just n -> (fromIntegral n)
		Nothing -> error "Character not in base 64"

convertWith :: ([Word6] -> [Word6]) -> String -> String
convertWith f = deserial . f . serial
