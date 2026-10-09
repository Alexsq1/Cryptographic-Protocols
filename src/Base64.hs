-- | Module for basic functions of base 64 integers and conversions between strings.

module Base64(serial, deserial, convertWith) where

import Data.ShortWord(Word6)
import Data.List(elemIndex)


-- | Converts string to numbers in base 64
serial :: String -> [Word6]
serial = map charToNum

-- | Converts numbers in base 64 to string 
deserial :: [Word6] -> String 
deserial = map numToChar

-- | Transformation between strings given a function between lists of numbers
convertWith :: ([Word6] -> [Word6]) -> String -> String
convertWith f = deserial . f . serial

-- | Possible characters in base 64
alphabet :: String
alphabet = ( ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['+' , '/'] )

-- auxiliars of 1 character

numToChar :: Word6 -> Char
numToChar n = alphabet !! (fromIntegral n)

charToNum :: Char -> Word6
charToNum c = 
            case (elemIndex c alphabet) of
	        Just n -> (fromIntegral n) 
                Nothing -> error "Character not in base 64"

