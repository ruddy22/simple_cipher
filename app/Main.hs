module Main where

import qualified Rot (encode, decode)
import qualified Otp (applyOTP)
import Cipher

data Rot = Rot
data OneTimePad = OTP String

instance Cipher Rot where
  encode Rot text = Rot.encode text
  decode Rot text = Rot.decode text

instance Cipher OneTimePad where
  encode (OTP pad) text = Otp.applyOTP pad text
  decode (OTP pad) text = Otp.applyOTP pad text

myPad :: OneTimePad
myPad = OTP (cycle [minBound .. maxBound])

main :: IO ()
main = do
    let message = "Hello, Haskell"
    print "Message (Rot):"
    print message
    print "================= RotN start ================="
    print "Encoded (Rot):"
    print (encode Rot message)
    print "Decoded (Rot):"
    print (decode Rot $ encode Rot message)
    print "================= RotN end ================="
    print "================= OneTimePad start ================="
    print "Encoded (OTP):"
    print (encode myPad message)
    print "Decoded (OTP):"
    print (decode myPad $ encode myPad message)
    print "================= OneTimePad end ================="
