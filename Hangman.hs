module Hangman where

import Control.Applicative
import System.Random
import Data.Char

hangman :: String -> [Char] -> IO ()
hangman word guessed
	| all (`elem` guessed) word = putStrLn "won"
	| length guessed > 5        = putStrLn ("Lost, the word was: " ++ word)
	| otherwise = do
		l <- toLower . head <$> prompt "Pick a letter:"
		let guessed'
			| l `elem` guessed = guessed
			| otherwise        = l:guessed
		putStrLn ("guessed = " ++ guessed)
		hangman word guessed'

main :: IO ()
main = do
	l <- read <$> prompt "Word length?"
	-- searching for words of a given length in the file
	chosenWords	<- filter ((l==) . length) <$> map init <$> lines <$> readFile "./enable1.txt"
	-- choosing one word from the list
	chosenWord <- (!!) chosenWords <$> randomRIO (0, length chosenWords)
	-- game
	hangman chosenWord []

prompt s = putStr (s ++ " ") >> getLine