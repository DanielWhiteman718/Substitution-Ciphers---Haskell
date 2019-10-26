module CipherAssignment1 where
import AssignmentHelp
import Data.List
import Data.Char

-- Declaring a String constant for the alphabet
alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

{- Takes a Cipher, sorts it into alphabetical order
   and checks if it's equal to the alphabet -}
validateCipher :: Cipher -> Bool
validateCipher cipher = sort cipher == alphabet

{-Testing validateCipher:
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" Output: True
Input: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" Output: True
Input: "ETAONIHSRLDUCMWYFGPBVKJXQZ" Output: True
Input: "A"                          Output: False
Input: "RLDUCMWYFGPBVKJ"            Output: False
-}





{- Gets the index of the letter in the alphabet,
   subtracts the offset, then returns the letter in
   that index of the cipher -}
encode :: Cipher -> Int -> Char -> Char
encode cipher offset char = cipher !! (((alphaPos char) - offset) `mod` 26)

{-Testing encode:
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 3 'M'    Output: 'Z'
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (-3) 'M' Output: 'H'
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (-3) 'B' Output: 'L'
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 6 'W'    Output: 'X'
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 'W'    Output: 'B'
-}





{- Calls encode on each character in the given
   String and then returns the encoded string -}
encodeMessage :: Cipher -> Int -> String -> String
encodeMessage cipher offset message = [encode cipher offset n | n <- message]

{-Testing encodeMessage:
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 3 "SPLIT"       Output: "HOVGX"
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (-3) "SHOP"     Output: "INUS"
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (-3) "BIRTHDAY" Output: "LTABNDFK"
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 6 "ILLUSTRATE"  Output: "MGGYOWTAWC"
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 "ILLUSTRATE"  Output: "VTTASPUEPL"
-}





{- Gets the index of the letter in the cipher,
   adds the offset value, then returns the letter in
   that index of the alphabet -}
reverseEncode :: Cipher -> Int -> Char -> Char
reverseEncode cipher offset char = alphabet !! (((fromMaybe (elemIndex char cipher)) + offset) `mod` 26)

{-Testing reverseEncode:
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 3 'Z'    Output: 'M'
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (-3) 'H' Output: 'M'
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (-3) 'L' Output: 'B'
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 6 'X'    Output: 'W'
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 'B'    Output: 'W'
-}





{- Calls reverseEncode on each character in the given
   String and then returns the decoded string -}
reverseEncodeMessage :: Cipher -> Int -> String -> String
reverseEncodeMessage cipher offset message = [reverseEncode cipher offset n | n <- message]

{-Testing reverseEncodeMessage:
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 3 "HOVGX"       Output: "SPLIT"
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (-3) "INUS"     Output: "SHOP"
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (-3) "LTABNDFK" Output: "BIRTHDAY"
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 6 "MGGYOWTAWC"  Output: "ILLUSTRATE"
Input: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 "VTTASPUEPL"  Output: "ILLUSTRATE"
-}





{- Helper function for letterStats, 
   Returns all instances of a character
   in a given string. Eg) Input: 'E' "ENGAGEMENT" Output: "EEE" -}
allInst :: Char -> String -> [Char]
allInst letter message = [n | n <- message, n == letter] 

{- Helper function for letterStats, 
   Returns the percentage of a certain
   character in a message. Do this by dividing the number
   of instances of the character by the number of characters in the message
   and then multiplying by 100. This function rounds the percentage
   to the nearest whole number -}
letterPerc :: String -> String -> Int
letterPerc message instances = percent (length instances) (length message)

{- Returns a list of each unique character in a String
   with its percentage of that string as a tuple.
   This makes use of the helper functions above -}
letterStats :: String -> [(Char, Int)]
letterStats message = sortBy (\(_,a) (_,b) -> compare b a) [(n, (letterPerc message (allInst n message))) | n <- nub message]

{-Testing letterStats:
Input: "SUNDAY"      Output: [('S',17),('U',17),('N',17),('D',17),('A',17),('Y',17)] 
Input: "ENGAGEMENT"  Output: [('E',30),('N',20),('G',20),('A',10),('M',10),('T',10)]
Input: "SHEET"       Output: [('E',40),('S',20),('H',20),('T',20)]
Input: "HYPOTHESIS"  Output: [('H',20),('S',20),('Y',10),('P',10),('O',10),('T',10),('E',10),('I',10)]
Input: "LOUD"        Output: [('L',25),('O',25),('U',25),('D',25)]
Input: "BLOODSHED"   Output: [('O',22),('D',22),('B',11),('L',11),('S',11),('H',11),('E',11)]
Input: "BBBB"        Output: [('B',100)]
Input: ""            Output: []
-}





{- Helper function for partialDecode, 
   Takes a tuple of two characters and makes the
   first character in the tuple lower case -}
lowerLetter :: (Char, Char) -> (Char, Char)
lowerLetter guess = ((toLower (fst guess)), snd guess)

{- Helper function for partialDecode, 
   Given a character and a list of guesses,
   identifies the guess that involves that character
   and returns it with the first character being in
   lowercase. If it can't find a guess that matches
   the character then it assumes the character should
   not change and returns a tuple of two characters,
   both of which are the character that was inputted -}
findGuess :: Char -> [(Char, Char)] -> (Char, Char)
findGuess letter guessList | (length guessList == 0) = (letter, letter)
                           | (letter == snd (guessList !! 0)) = lowerLetter (guessList !! 0)
                           | otherwise = findGuess letter (tail guessList)

{- Given a list of guesses and a message, returns the message 
   where the characters that have guesses are replaced by lower 
   case versions of the character they are guessed to be -}
partialDecode :: [(Char, Char)] -> String -> String
partialDecode guessList message = [fst (findGuess n guessList) | n <- message]

{-Testing partialDecode:
Input: [('E','X'),('S','W')] "DXPWXW"                       Output: "DePses"
Input: [] "DXPWXW"                                          Output: "DXPWXW"
Input: [('S','A'),('T','J'),('O','F'),('P','V')] "EZAAJFV"  Output: "EZsstop"
Input: [('H','P'),('E','J'),('L','O')] "TPXEJO"             Output: "ThXEel"
Input: [('D','B'),('Z','G')] "BJBWGG"                       Output: "dJdWzz"
Input: [] "FSXFHJ"                                          Output: "FSXFHJ" 
-}

{- ITS EASY TO BREAK A SUBSTITUTION CIPHER PROVIDED YOU HAVE A LONG ENOUGH MESSAGE STOP LETS MAKE
   THIS ONE A LITTLE BIT LONGER STOP OK IT SHOULD BE THE RIGHT SORT OF SIZE NOW STOP MAYBE NOT LETS 
   INCREASE THE MESSAGE LENGTH ABIT MORE STOP KEEP THIS MESSAGE SECRET OR SHARE IF YOU WANT THE
   WHOLE CLASS TO GET THE BONUS MARKS STOP -}