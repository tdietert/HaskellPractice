import Control.Applicative
import Data.List

data WordBullet = WordBullet {word::String,debris::String}

printScore :: [WordBullet] -> String
printScore bullets@[aBullet,bBullet]
    | aScore > bScore = unwords [a,"dominated",b,"!",totalDebris,"fell into the valley!"]
    | aScore < bScore = unwords [b,"dominated",a,"!",totalDebris,"fell into the valley!"]
    | otherwise = unwords [a,"and",b,"Tie!",totalDebris,"fell into the valley!"]
    where [a,b] = map (show . word) bullets
    	  [aScore,bScore] = map (length . debris) bullets
    	  [aRem,bRem] = map (show . debris) bullets
    	  totalDebris = intercalate " and " [aRem,bRem]

explodeWords :: String -> String -> [WordBullet]
explodeWords a b = [WordBullet a aRem, WordBullet b bRem]
	where [aRem,bRem] = zipWith (\\) [a,b] [b,a]

main = do
	[a,b] <- words <$> getLine
	putStrLn $ printScore $ explodeWords a b