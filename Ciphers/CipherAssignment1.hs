module CipherAssignment1 where
import AssignmentHelp
import Data.List

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

reverseEncodeMessage :: Cipher -> Int -> String -> String
reverseEncodeMessage cipher offset message = [reverseEncode cipher offset n | n <- message] 

removeDuplicate :: String -> String
removeDuplicate [] = []
removeDuplicate [a] = [a]
removeDuplicate text = head text:(removeDuplicate (delete (head text) (tail text)))

allInst :: Char -> String -> [Char]
allInst letter message = [n | n <- message, n == letter] 

letterPerc :: String -> String -> Int
letterPerc message instances = quot ((length instances)*100) (length message)

statTuple :: Char -> Int -> (Char, Int)
statTuple letter percentage = (letter, percentage)

letterStats :: String -> [(Char, Int)]
letterStats message = sortBy (\(_,a) (_,b) -> compare b a) [statTuple n (letterPerc message (allInst n message)) 
                                                                            | n <- removeDuplicate message]

