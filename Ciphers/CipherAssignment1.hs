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