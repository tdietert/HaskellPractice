import Control.Applicative
import Control.Monad
import Data.Char

toDigitList :: String -> [Integer]
toDigitList = map (toInteger . digitToInt) 

convFromDecToBase :: Integer -> Integer -> Integer -> String
convFromDecToBase base n_state n = buildNew (n*n_state) "" bases
    where bases = reverse $ takeWhile (\x -> x <= abs n) $ zipWith (^) (repeat base) [0..] 
          buildNew :: Integer -> String -> [Integer] -> String
          buildNew 
          

convFromBaseToDec :: Integer -> Integer -> [Integer] -> Integer
convFromBaseToDec base n_state n = smashToDec bases
     where powers = reverse [0..((toInteger $ length n)-1)]
           bases = take (length n) $ repeat base  
           smashToDec x = (*) n_state . sum . zipWith (*) n . zipWith (^) x $ powers

main = forever $ do
    [base,n] <- words <$> getLine
    let n_state = if head n == '-' then -1 else 1
        newN = if head n == '-' then tail n else n
        res = convFromBaseToDec (read base) n_state . toDigitList $ newN
        r_state = if res < 0 then -1 else 1
    print . convFromBaseToDec (read base) n_state . toDigitList $ newN
    print . convFromDecToBase ((-1)*(read base)) r_state $ res
