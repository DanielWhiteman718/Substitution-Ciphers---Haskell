import AssignmentHelp
import Data.List
--import Data.Maybe

alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

validateCipher :: Cipher -> Bool
validateCipher cipher = sort cipher == alphabet

encode :: Cipher -> Int -> Char -> Char
encode cipher offset char = cipher !! (((alphaPos char) + offset) `mod` 26)

encodeMessage :: Cipher -> Int -> String -> String
encodeMessage cipher offset message = [encode cipher offset n | n <- message]

reverseEncode :: Cipher -> Int -> Char -> Char
reverseEncode cipher offset char = alphabet !! ((fromMaybe (elemIndex char cipher)) - offset)
