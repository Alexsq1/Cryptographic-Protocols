module Main (main) where

import Test.QuickCheck
import Base64Test
import CaesarTest
import VigenTest
import RSATest

main :: IO ()
main = do 
          putStrLn "Testing base 64:"
	  base64Test

          putStrLn "Testing Caesar:"
	  caesarTest

          putStrLn "Testing Vigenere:"
	  vigenTest

          putStrLn "Testing RSA:"
	  rsaTest
