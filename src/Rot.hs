module Rot  ( encode
            , decode
            )
where 

rotEncode :: (Bounded a, Enum a) => Int -> a -> a
rotEncode alphabetSize ch = toEnum rotation
  where
    halfAlphabet = div alphabetSize 2
    offset = fromEnum ch + halfAlphabet
    rotation = mod offset alphabetSize

rotDecode :: (Bounded a, Enum a) => Int -> a -> a
rotDecode alphabetSize ch = toEnum rotation
  where
    halfAlphabet = div alphabetSize 2
    offset = if even alphabetSize
             then fromEnum ch + halfAlphabet
             else 1 + fromEnum ch + halfAlphabet
    rotation = mod offset alphabetSize

encode :: (Bounded a, Enum a) => [a] -> [a]
encode vals = map process vals
  where
    process = rotEncode largestNumber
    largestNumber = 1 + fromEnum (maxBound :: Char)

decode :: (Bounded a, Enum a) => [a] -> [a]
decode vals = map process vals
  where
    process = rotDecode largestNumber
    largestNumber = 1 + fromEnum (maxBound :: Char)