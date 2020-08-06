module Cipher where

class Cipher a where
  encode :: a -> [Char] -> [Char]
  decode :: a -> [Char] -> [Char]