-- Priority Qwayway (queue)

data PQueue = Empty --    val  p1     p2      A       B
            | Heap Int Int a Double Double (PQueue,PQueue)

empty :: PriorityQueue -> Bool
empty pq = 

rankA :: PriorityQueue -> Int
rankA Empty = 0
rankA (PriorityQueue rA _ _ _ _ _) = rA

rankB :: PriorityQueue -> Int
rankB Empty = 0
rankB (PriorityQueue _ rB _ _ _ _) = rB

combinePQueueA :: Double -> PriorityQueue
combinePQueueA x a b = if rankA a > rankB b 
	                       then PriorityQueue (rankB b + 1)

combinePQueueB

-- merge (very fast operation O(log(min rank of heap a and heap b)))
mergeA :: PriorityQueue -> PriorityQueue -> PriorityQueue
mergeA Empty heap2 = heap2
mergeA heap1 Empty = heap1
mergeA (r,p1,p2,heap1,heap2)

mergeB :: PriorityQueue -> PriorityQueue -> PriorityQueue
mergeB Empty heap2 = heap2
mergeB heap1 Empty = heap1
mergeB pq1@(Heap aR1 bR1 x aP1 bP1 (aPQ1,bPQ1)) pq2@(Heap aR2 bR2 y aP2 bP2 (aPQ1,apQ2)) =
	if bP1 <= bP2 then Heap