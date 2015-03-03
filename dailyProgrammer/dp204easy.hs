import Control.Monad
import Data.String.Utils
import Data.List

divide :: [String] -> [[String]]
divide [] = [[]]
divide c@(s:script) = if section /= [] then (map lstrip $ section):(divide rest) else divide script
	where (section,rest) = span (startswith "    ") c

search :: String -> [[String]] -> String
search [] _ = "Can't search for an empty String!"
search x [] = "Can't search an empty String!"
search x (s:script) = if or (map (x `isInfixOf`) s) then unlines s else search x (script)
	        
main = do 
	contents <- liftM lines $ readFile "macbeth.txt"
	putStrLn "Please enter a word or phrase to find the passage in Macbeth that contains it:"
	wordToFind <- getLine
	putStr $ search wordToFind (divide contents)
	


