module Hangman where

import Control.Applicative
import System.Random
import Data.Char

hangman :: String -> [Char] -> IO ()
hangman word lettersUsed
	| all (`elem` lettersUsed) word        = putStrLn ("won, the word was: " ++ word)
	| (length $ lettersMissed word lettersUsed) > 5    = putStrLn ("Lost, the word was: " ++ word)
	| otherwise = do
		putStrLn ("the word: " ++ (hide word lettersUsed))

		putStrLn ("letters missed: " ++ lettersMissed word lettersUsed)

		l <- toLower . head <$> prompt "Pick a letter:"

		let lettersUsed'
			| l `elem` lettersUsed = lettersUsed
			| otherwise            = l:lettersUsed

		if (length lettersUsed') > 0
			then putStrLn ("lettersUsed = " ++ [x | x <- lettersUsed', not $ x `elem` word])
			else return ()

		hangman word lettersUsed'

main :: IO ()
main = do
	l <- read <$> prompt "Word length?"
	-- searching for words of a given length in the file
	chosenWords	<- filter ((l==) . length) <$> map init <$> lines <$> readFile "./enable1.txt"
	-- choosing one word from the list
	chosenWord <- (!!) chosenWords <$> randomRIO (0, length chosenWords)
	-- start game loop
	hangman chosenWord []

prompt s = putStr (s ++ " ") >> getLine

hide :: String -> String -> String
hide word guesses = map (\x -> if x `elem` guesses then x else '-') word

lettersMissed :: String -> String -> [Char]
lettersMissed word letters = [x | x <- letters, not $ x `elem` word]