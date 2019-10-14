import AssignmentHelp
import Data.List

oddx :: Int -> Bool
oddx n = n `mod` 2 /= 0

oddItems :: [Int] -> [Int]
oddItems [] = []
oddItems numbers = [n | n <- numbers, oddx n]


alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

validateCipher :: Cipher -> Bool
validateCipher cipher = sort cipher == alphabet

encode :: Cipher -> Int -> Char -> Char
encode cipher offset char = cipher !! (((alphaPos char) + offset) `mod` 26)

encodeMessage :: Cipher -> Int -> String -> [Char]
encodeMessage cipher offset message = map f 
                                      where f x = encode cipher offset x

