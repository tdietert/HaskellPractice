import Control.Parallel
import System.Time

parFib :: Int -> Integer
parFib = (fib !!)
    where fib = 0:1:(zipWith (+) (tail fib) fib)

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
	fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

r1 :: Integer
r1 = parFib 100

main :: IO ()
main = do 
    t0 <- getClockTime
    pseq r1 (return ())
    t1 <- getClockTime
    putStrLn ("sum: " ++ show r1)
    putStrLn ("time: " ++ show (secDiff t0 t1) ++ "seconds")