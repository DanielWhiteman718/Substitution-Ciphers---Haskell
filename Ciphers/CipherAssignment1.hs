import AssignmentHelp
import Data.List

oddx :: Int -> Bool
oddx n = n `mod` 2 /= 0

alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

validateCipher :: Cipher -> Bool
validateCipher cipher = sort cipher == alphabet

encode :: Cipher -> Int -> Char -> Char
encode cipher offset char = cipher !! (((alphaPos char) + offset) `mod` 26)