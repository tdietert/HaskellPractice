import Control.Applicative
import Data.List
import qualified Data.Map.Strict as Map

data Street = Street { streetName :: String, endPoints :: String, times :: [Int] }
    deriving (Show, Ord, Eq)

data Intersection = Intersection { name :: Char, conns :: [Street] } 
    deriving (Show)

buildStreets :: [String] -> [Street]
buildStreets streetData = map buildStreet streetData
	where buildStreet :: String -> Street
	      buildStreet streetInfo = Street streetName (concat [a,b]) times
	          where parsedInfo = words streetInfo
	                [a,b] = take 2 parsedInfo
	                times = map read $ drop ((length parsedInfo)-4) parsedInfo
	                streetName = unwords $ drop 2 $ take ((length parsedInfo)-4) parsedInfo

buildInters :: [Street] -> String -> [Intersection]
buildInters streets [] = []
buildInters streets (i:inters) = (Intersection i currInterConns):(buildInters streets inters)
    where currInterConns = helper streets
          helper = foldl (\acc street@(Street _ [a,b] _) -> if a==i || b==i then street:acc else acc) [] 

getTime :: Int -> Int
getTime time 
    | timeInt <= 800  = 1
    | timeInt <= 1200 = 2
    | timeInt <= 1800 = 3
    | otherwise       = 4
    where timeInt =time

getDest :: Intersection -> Street -> Char
getDest currInter street = if name currInter == a then b else a
    where [a,b] = endPoints street

djikstras :: Char -> Char -> Int -> [Intersection] -> (Map.Map Char Int, Map.Map Char Char)
djikstras start end time network = djIter dists prevs network
    where dists = Map.fromList $ zipWith initDists (map name network) (repeat (maxBound :: Int))
          prevs = Map.fromList $ zip (map name network) (repeat ' ')
          initDists = (\a b -> if a == start then (a,0) else (a,b)) 

getMaybeVal :: Maybe a -> a
getMaybeVal (Maybe x) -> x

updateNeighbors d p curr [] = (d,p)
updateNeighbors d p curr (n:ns) =
    where alt = (getMaybeVal (Map.lookup (name curr) d)) + (map)


djIter :: (Map.Map Char Int) -> (Map.Map Char Char) -> [Intersection] -> (Map.Map Char Int, Map.Map Char Char)
djIter d p [] = (d,p)
djIter d p network = 
    where u = head network
          neighsAndLens = zip (map (getDest u) (conns u)) (map (flip (!!) 4) (map times (conns u)))

main = do
    contents <- readFile "dpStreetData197.txt"
    inputs <- words <$> getLine
    let [start, end, time] = read <$> inputs
        streets = buildStreets $ lines contents
        intersectNames = sort . nub . concat $ map endPoints streets
        roads = buildInters streets intersectNames
        shortestPath = djikstras start end (getTime 3) roads
    print shortestPath
