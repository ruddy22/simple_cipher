module Otp (applyOTP) where

xorBool :: Bool -> Bool -> Bool
xorBool x y = (x || y) && (not (x && y))

xorPair :: (Bool, Bool) -> Bool
xorPair (x, y) = xorBool x y

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (reminder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
               where
                 reminder = mod n 2
                 nextVal = div n 2

maxBits :: Int
maxBits = length $ intToBits' maxBound

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
              where
                reversedBits = reverse $ intToBits' n
                missingBits = maxBits - (length reversedBits)
                leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits ch = intToBits $ fromEnum ch

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where
    size = length bits
    indices = [size-1, size-2 .. 0]
    trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum $ bitsToInt bits

applyOTP' :: [Char] -> [Char] -> [Bits]
applyOTP' pad plainText = map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits plainTextBits)
  where
    padBits = map charToBits pad
    plainTextBits = map charToBits plainText

applyOTP :: [Char] -> [Char] -> [Char]
applyOTP pad plainText = map bitsToChar bits
  where
    bits = applyOTP' pad plainText

secretPad :: [Char]
secretPad = "SuperPuperSecretPad!!!!!"

encodeDecode :: [Char] -> [Char]
encodeDecode str = applyOTP secretPad str