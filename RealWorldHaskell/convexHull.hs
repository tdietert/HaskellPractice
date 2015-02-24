import Data.Function
import Data.List

data Direction = LeftTurn | RightTurn | Straight
                 deriving (Show, Eq)

getTurn :: [(Int,Int)] -> Direction
getTurn [a,b,c]
    | calc > 0 = LeftTurn
    | calc < 0 = RightTurn
    | otherwise = Straight
    where calc = (fst b - fst a)*(snd c - snd a)-(snd b - snd a)*(fst c - fst a)
getTurn _ = error "Function takes 3 points"

sortByY :: [(Int, Int)] -> [(Int, Int)]
sortByY xs = (sortBy (compare `on` fst) sameX) ++ rest
    where sortedY = sortBy (compare `on` snd) xs
          (sameX,rest) = break (\(x,y) -> y == snd (head sortedY)) sortedY
          
sortByAngle :: [(Int,Int)] -> [(Int,Int)]
sortByAngle xs = (fstX,fstY):sortedPoints
    where (fstX,fstY) = head xs
          rest = tail xs
          sortedPoints = sortBy (compare `on` (calcAngle (fstX,fstY))) rest

calcAngle :: (Int,Int) -> (Int,Int) -> Double
calcAngle (x1,y1) (x2,y2) = (atan2 deltaY deltaX)*(180 / pi)
	where deltaX = fromIntegral $ x2 - x1
	      deltaY = fromIntegral $ y2 - y1
calcAngle _ _ = error "Must give calcAngle 2 points"
          
pointDist :: (Int,Int) -> (Int,Int) -> Double
pointDist (x1,y1) (x2,y2) = sqrt $ xs + ys
    where xs = (fromIntegral (x2 - x1))**2
          ys = (fromIntegral (y2 - y1))**2
pointDist _ _ = error "pointDist takes two pairs as args"

perimeter :: [(Int,Int)] -> Double
perimeter [] = 0
perimeter [x] = 0
perimeter (x:y:ys) = (pointDist x y) + (perimeter (y:ys)) 

solve :: [(Int, Int)] -> Double
solve points =  perimeter . scan $ sortedPointsAngle ++ [head sortedPointsY]
    where sortedPointsY = sortByY points
          sortedPointsAngle = sortByAngle sortedPointsY
          scan (a:b:c:rest)
              | turn == RightTurn = scan (a:c:rest)
              | otherwise = a : scan (b:c:rest)
                where turn = getTurn [a,b,c]
          scan xs = xs

--solve :: [(Int, Int)] -> [(Int,Int)]
--solve points =  scan sortedPoints ++ [head sortedPoints]
--    where sortedPoints = sortByCotan $ sortByY points
--          scan (a:b:c:rest)
--              | turn == RightTurn = scan (a:c:rest)
--              | otherwise = a : scan (b:c:rest)
--                where turn = getTurn [a,b,c]
--          scan _ = []