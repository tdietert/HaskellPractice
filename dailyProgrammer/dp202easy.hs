import Control.Applicative
import Data.List.Split
import System.Environment
import Data.Char

-- I'm so stupid I had to look at a spoiler to know it was 8-bit ascii
ascii :: String -> String
ascii = map (chr . raiseDigits) . chunksOf 8 . map digitToInt
    where raiseDigits x = sum . getZipList $ ((*) . (2^)) <$> ZipList [7,6..] <*> ZipList x

main = do
	(input:args) <- getArgs
	print $ ascii input

