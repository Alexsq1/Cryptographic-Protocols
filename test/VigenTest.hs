module VigenTest(vigenTest) where

import Test.QuickCheck
import Data.ShortWord(Word6)
import Base64Test
import Vigenere

vigenTest :: IO ()
vigenTest = do 
          putStrLn "\nTesting Vigenere:"
	  quickCheck (vigenCorrectness)
	  quickCheck (unsafeKey)

-- | Vigenere Correctness
vigenCorrectness :: Property
vigenCorrectness = forAll genString64 (\m -> 
			forAll genString64 (\k -> 
				decrypt k (encrypt k m) === m))

-- | key 0 is unsafe
unsafeKey :: Property
unsafeKey = forAll genString64 (\m -> 
		forAll keyA (\k ->
			encrypt k m === m))

-- | Generator of strings only conformed with 'A' (unsafe key)
keyA :: Gen String
keyA = listOf (elements ['A'] )
