module Amy where 
import Utils 
import Structure 

initialBFS ::Environment -> Int -> Int -> [(Agent,[(Int,Int)])]  
initialBFS envi i j = bfs envi 0 [((i,j),[(i,j)])] [(i,j)] []

bfs :: Environment -> Int -> [((Int,Int),[(Int,Int)])] -> [(Int,Int)]-> [(Agent,[(Int,Int)])]-> [(Agent,[(Int,Int)])]
bfs envi pos pending  visited result =  if pos /= ((number_Rows envi) * ( number_Columns envi))
                                        then let (x, way) = pending!!pos
                                                 i = fst x
                                                 j = snd x
                                              in  if validateAdjacent envi i j
                                                  then let (new_visited, new_pending) =  expand envi pos visited pending  
                                                        in if not ( verifyIsEmpty envi i j)
                                                           then  bfs envi (pos + 1) new_pending new_visited (result ++ [(returnAgent envi i j, way )])
                                                           else bfs envi (pos + 1) new_pending new_visited result
                                                  else bfs envi (pos + 1) pending visited result 
                                        else result                    

expand :: Environment -> Int -> [(Int, Int)] -> [((Int,Int),[(Int,Int)])] -> ([(Int, Int)],[((Int,Int),[(Int,Int)])])
expand envi pos  visited [] = (visited,[])
expand envi pos  visited pending = let (x, way) = pending!!pos  
                                       i = fst x
                                       j = snd x 
                                       adjacents = traceShow((i,j,"esssss"))( generateAdjacents i j)
                                       y = inBoard envi adjacents 
                                       (new_list, new_visited ) = isVisited visited y []
                                       new_pending = traceShow((new_list,"adddddd")) (addAdjacentPending new_list pos pending )
                                     in ( new_visited, new_pending)                                   


isVisited:: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
isVisited visited [] result =  (result, visited) 
isVisited visited (adj: xs) result  = if adj `elem` visited
                              then isVisited visited xs result 
                              else isVisited (visited ++ [adj]) xs (result ++ [adj])

addAdjacentPending :: [(Int, Int)] -> Int -> [((Int,Int),[(Int,Int)])] -> [((Int,Int),[(Int,Int)])] 
addAdjacentPending  [] pos  pending  = pending
addAdjacentPending (adj: xs) pos pending =  let (position , way) = pending!!pos
                                                new = (adj, way ++ [adj]) 
                                                   in addAdjacentPending xs pos (pending ++ [new])                                             


generateAdjacents :: Int -> Int -> [(Int,Int)]
generateAdjacents i j = let x1  = (i - 1 , j ) 
                            x2 = (i , j + 1)      
                            x3 = (i + 1, j )
                            x4= ( i , j - 1 )
                            in [x1,x2,x3,x4] 

inBoard :: Environment -> [(Int,Int)] -> [(Int,Int)] 
inBoard envi [] = []
inBoard envi (x:xs) = if  verifyInBoard envi (fst x) (snd x)  then [x] ++ inBoard envi xs  else  inBoard envi xs                       

--validateAdjacents :: Environment -> [(Int,Int)] -> [(Int,Int)]
--validateAdjacents envi []  = [] 
--validateAdjacents envi (adj:xs) =  if validateAdjacent envi (fst adj)  (snd adj)
                                   --then [adj] ++ validateAdjacents envi xs 
                                 --  else validateAdjacents envi xs 

validateAdjacent:: Environment -> Int -> Int -> Bool
validateAdjacent envi i j  = if verifyIsEmpty envi i j 
                            then True
                            else if  elementBelongs (robots envi) i j || elementBelongs (obstacles envi) i j
                                then False 
                                else if  haveLoadedChild envi (Agent "Robot" i j) &&  elementBelongs (children envi) i j &&  elementBelongs (corrals envi) i j
                                     then False 
                                     else True 
      

returnAgent:: Environment -> Int -> Int -> Agent
returnAgent envi i j = let item = [ x | x  <- children envi ,row x == i , column x == j ]
                        in if length item > 0 then item!!0 else let item1  = [ x | x  <- corrals envi ,row x == i , column x == j ] 
                                                                in if length item1 > 0 then item1!!0 
                                                                                       else let item0  = [ x | x  <- corrals envi ,row x == i , column x == j ]     
                                                                                            in item0!!0
