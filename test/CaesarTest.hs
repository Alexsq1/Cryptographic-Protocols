module CaesarTest(caesarTest) where

import Test.QuickCheck
import Data.ShortWord(Word6)
import Base64Test
import Caesar

caesarTest :: IO ()
caesarTest = do 
          putStrLn "\nTesting Caesar correctness:"
	  quickCheck (caesarCorrectness)
	  quickCheck (unsafeKey)

-- | caesarCorrectness
caesarCorrectness :: Word6 -> Property
caesarCorrectness k = forAll genString64 (\m -> decrypt k (encrypt k m) === m)

-- | key 0 is unsafe
unsafeKey :: Property
unsafeKey = forAll genString64 (\m -> encrypt 0 m === m)





