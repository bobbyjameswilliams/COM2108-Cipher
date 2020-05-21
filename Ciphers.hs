
-- Bobby Williams Haskell Assignment 2019
import AssignmentHelp
import Data.Map
import Data.Maybe
import Data.Char
import Data.List
import Data.Function (on)

--Used as a string which definitely contains all letters of the alpabet
alphabetComparitor :: String
alphabetComparitor = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

--validates that the cipher contains one and only one of every character in the alphabet.
{- 
Limitations:
    - only capital ciphers 
Tests
1)
Input: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
Output: True
Input:"AACDEFGHIJKLMNOPQRSTUVWXYZ"
Output: False
-}
validateCipher :: Cipher -> Bool
validateCipher cipherToComp = ((sort cipherToComp) == alphabetComparitor)

--takes cipher offset and target letter and encodes using the given cipher
{-
Limitations:
    - Only capital letter characters will correctly encode with valid ciphers
    -Negative offsets need brackets
    - No spaces
1)
Input: {
    origCip = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    offset = 2
    origChar = 'A'
Output = 'C'
2)
Input: {
    origCip = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    offset = (-2)
    origChar = 'A'
Output = M
}
-}

encode :: Cipher -> Int -> Char -> Char
encode origCip offset origChar = origCip!!(((alphaPos origChar) - offset) `mod` 26)

-- using encode, encodes the message taking the cipher shift and message as parameters
{-
Limitations:
    - Only capital letters will correctly encode with valid ciphers
    - Negative offsets need brackets
    - No spaces
1)
Input: {
    origCip = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    offset = 2
    message = "HELLO"
Output = 'GMZZO'
}
-}
encodeMessage :: Cipher -> Int -> String -> String
encodeMessage origCip offset message =  Data.List.map (encode origCip offset) message

--Does the reverse of the encode function in that it takes a cipher and encrypted character then returns the plaintext
{-
Limitations:
    - Only capital letters will correctly encode with valid ciphers
    - Negative offsets need brackets
    - No spaces
1)
Input: {
    origCip = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    offset = 2
    origChar = 'C'
Output = 'A'
2)
Input: {
    origCip = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    offset = (-2)
    origChar = 'M'
Output = A
}
-}

reverseEncode ::  Cipher -> Int -> Char -> Char
reverseEncode origCip offset origChar = chr ((((fromJust (elemIndex origChar origCip)) + offset) `mod` 26) + ord 'A')

--Using reverseEncoded, decodes an encoded message.
{-
Limitations:
    - Only capital letters will correctly encode with valid ciphers
    - Negative offsets need brackets
    - No spaces
1)
Input: {
    origCip = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    offset = 2
    message = "GMZZO"
Output = "HELLO"
}
-}

reverseEncodeMessage :: Cipher -> Int -> String -> String
reverseEncodeMessage origCip offset message =  Data.List.map (reverseEncode origCip offset) message

--Produces statistics of a string of letters, omitting zero and ordering them in order of descending order with the percentages
{-
Limitations:
    - Only capital letters will produce stats
    - No spaces in the text
1)
Input: "HELLOMYNAMEISBOBBYWILLIAMS"
Output: [('L',15),('B',12),('I',12),('M',12),('A',8),('E',8),('O',8),('S',8),('Y',8),('H',4),('N',4),('W',4)]
-}
letterStats :: String -> [(Char,Int)]
letterStats message = Data.List.filter ((/=0).snd)(sortBy (flip compare `on` snd) (createStats message))
--function used in the process of letterstats, filters the letter and returns a list of the amount of the letter in the list.
filterLetter :: String -> Char -> (Char,Int)
filterLetter message origChar =  (origChar, (percent ((length (Data.List.filter (== origChar) message))) (length message)))

--maps the filter function onto the string
createStats :: String -> [(Char,Int)]
createStats message = Data.List.map(filterLetter message) alphabetComparitor

--With a list of potential mappings, partialDecode takes guesses and produces a message with a partial cipher.
{-
Limitations:
    - Only capital letters in the message and guesses
    - No spaces in the text
1)
Input: "[('E','X),('S','W')]", "DXPWXW"
Output = DePses
-}
partialDecode :: [(Char,Char)] -> String -> String
partialDecode guesses message = Data.List.map(mapGuess guesses) message

{- Secret Message: ITS EASY TO BREAK A SUBSTITUTION CIPHER PROVIDED YOU HAVE A LONG ENOUGH MESSAGE STOP LETS MAKE THIS ONE A LITTLE BIT LONGER STOP OK IT SHOULD BE THE RIGHT SORT OF SIZE NOW STOP MAYBE NOT LETS INCREASE THE MESSAGE LENGTH A BIT MORE STOP KEEP THIS MESSAGE SECRET OR SHARE IF YOU WANT THE WHOLE CLASS TO GET THE BONUS MARKS STOP
-}

--function to be mapped onto the message to produce the message.
mapGuess :: [(Char,Char)] -> Char -> Char
mapGuess guesses origChar 
    | (isNothing guessQuery) == False = toLower (fst(guesses!!(fromJust(guessQuery))))
    | otherwise = origChar  
        where guessQuery = (elemIndex (origChar) (sndList guesses))

--returns a list of the first elements of a list of tuples
sndList :: [(Char,Char)] -> [Char]
sndList guesses = Data.List.map(sndElem) guesses
--function which is mapped onto the guesses, returns the first element in a tuple.
sndElem :: (Char,Char) -> (Char)
sndElem tuple = snd tuple 

