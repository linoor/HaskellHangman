module Hangman where

import Control.Applicative
import System.Random
import Data.Char

hangman :: String -> [Char] -> IO ()
hangman word lettersUsed
	| all (`elem` lettersUsed) word                                           = putStrLn ("You win! the word was: " ++ word)
	| (length $ lettersMissed word lettersUsed) >= (length hangmanDrawing)    = putStrLn ("You lose! the word was: " ++ word)
	| otherwise = do
		putStrLn ("the word: " ++ (hide word lettersUsed))

		putStrLn ("letters missed: " ++ lettersMissed word lettersUsed)

		l <- toLower . head <$> prompt "Pick a letter:"

		if (length lettersUsed) > 0
			then do
				putStrLn ("lettersUsed = " ++ [x | x <- lettersUsed, not $ x `elem` word])
				putStrLn $ hangmanDrawing !! (length $ lettersMissed word lettersUsed)
			else return ()

		let lettersUsed'
			| l `elem` lettersUsed = lettersUsed
			| otherwise            = l:lettersUsed

		-- printing hangman

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

prompt :: String -> IO String
prompt s = putStr (s ++ " ") >> getLine

hide :: String -> String -> String
hide word guesses = map (\x -> if x `elem` guesses then x else '-') word

lettersMissed :: String -> String -> [Char]
lettersMissed word letters = [x | x <- letters, not $ x `elem` word]

hangmanDrawing = [ unlines  [ ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     ]
          , unlines  [ ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , ""
                     , " ___"
                     ]
          , unlines  [ ""
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|"
                     , "|"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|        |"
                     , "|        |"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|       /|"
                     , "|        |"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|       /|\\"
                     , "|        |"
                     , "|"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|       /|\\"
                     , "|        |"
                     , "|       /"
                     , "|"
                     , "|___"
                     ]
          , unlines  [ "_________"
                     , "|/       |"
                     , "|       (_)"
                     , "|       /|\\"
                     , "|        |"
                     , "|       / \\"
                     , "|"
                     , "|___"
                     ]
          ]