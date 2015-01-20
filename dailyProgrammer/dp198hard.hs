import Control.Applicative
import Data.List

primeSieve :: Integer -> [Integer]
primeSieve m = sieve [2..m]
    where sieve (p:xs)
              | p*p > m   = p:xs
              | otherwise = p : sieve [x | x<-xs, rem x p /= 0] 

-- modified 
primeFactsLess20 :: Integer -> [Integer] -> Bool
primeFactsLess20 m primes = factorM m primes
	where factorM 1 _ = True
	      factorM _ [] = False
	      factorM x (p:ps) 
	          | rem x p == 0 = factorM (div x p) (p:ps)
	          | otherwise    = factorM x ps

millionth :: Integer -> Integer -> Integer
millionth x 10000 = x
millionth x c
    | (primeFactsLess20 x sieve) = millionth (x+1) (c+1) 
    | otherwise = millionth (x+1) c
      where sieve = primeSieve 20

main = do
	--input <- read <$> getLine
	let x = millionth 2 1
	print $ primeSieve 20
	print x

