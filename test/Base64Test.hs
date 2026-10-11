module Base64Test(base64Test, genString64) where

import Test.QuickCheck
import Data.ShortWord(Word6)
import Base64

base64Test :: IO ()
base64Test = do 
          putStrLn "\nTesting Base64:"
	  --quickCheck (charNum1)
	  --quickCheck (charNum2)
	  quickCheck (serial1)
	  quickCheck (serial2)

instance Arbitrary Word6 where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Int)

-- | charToNum and numToChar are inverses (tested, eliminated for refactoring)
{-

genB64 :: Gen Char
genB64 = elements (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['+' , '/'])

charNum1 :: Word6 -> Property
charNum1 n = charToNum (numToChar n) === n

charNum2 :: Property
charNum2 = forAll genB64 (\n -> numToChar (charToNum n) === n)
-}

-- | charToNum and numToChar are inverses

genString64 :: Gen String
genString64 = listOf (elements (
	 ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['+' , '/'] ))

serial1 :: [Word6] -> Property
serial1 n = serial (deserial n) === n

serial2 :: Property
serial2 = forAll genString64 (\n -> deserial (serial n) === n)



