module CipherAssignment1 where
import AssignmentHelp
import Data.List
import Data.Char

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

allInst :: Char -> String -> [Char]
allInst letter message = [n | n <- message, n == letter] 

letterPerc :: String -> String -> Int
letterPerc message instances = quot ((length instances)*100) (length message)

statTuple :: Char -> Int -> (Char, Int)
statTuple letter percentage = (letter, percentage)

letterStats :: String -> [(Char, Int)]
letterStats message = sortBy (\(_,a) (_,b) -> compare b a) [statTuple n (letterPerc message (allInst n message)) 
                                                                            | n <- nub message]

lowerLetter :: (Char, Char) -> (Char, Char)
lowerLetter guess = ((toLower (fst guess)), snd guess)


findGuess :: Char -> [(Char, Char)] -> (Char, Char)
findGuess letter guessList | (length guessList == 0) = (letter, letter)
                           | (letter == snd (guessList !! 0)) = lowerLetter (guessList !! 0)
                           | otherwise = findGuess letter (tail guessList)

replaceChar :: [(Char, Char)] -> Char -> Char
replaceChar guessList letter = fst (findGuess letter guessList)


partialDecode :: [(Char, Char)] -> String -> String
partialDecode guessList message = [replaceChar guessList n | n <- message]

lsMystery :: Char -> [(Char, Int)] 
lsMystery letter = letterStats mystery
