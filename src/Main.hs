module Main where       
import Debug.Trace
import Data.List
import System.Random 
import Structure
import Board
import Utils
createInitialEnvironment :: Int -> Int -> Int -> Int -> [[Agent]]-> [Agent]  ->  Environment
createInitialEnvironment number_rows number_columns t t_final agentss hold_child =  Environment { robots = agentss!!0,
                                                                                                  children = agentss!!1,
                                                                                                  obstacles =  agentss!!3,
                                                                                                  corrals = agentss!!2,
                                                                                                  dirt = agentss!!4,
                                                                                                  holdChild = hold_child,
                                                                                                  number_Rows =  number_rows,
                                                                                                  number_Columns = number_columns,
                                                                                                  t_Rnd = t,
                                                                                                  t_Final = t_final}
                                           
changeTimeEnvironment :: Environment -> Int -> Int -> Environment 
changeTimeEnvironment envi t t_final = Environment {robots = robots envi,
                                                    children = children envi,
                                                    obstacles =  obstacles envi,
                                                    corrals = corrals envi,
                                                    dirt = dirt envi,
                                                    holdChild = holdChild envi,
                                                    number_Rows =  number_Rows envi ,
                                                    number_Columns = number_Columns envi,
                                                    t_Rnd = t,
                                                    t_Final = t_final}

updateEnvironment :: Environment -> String -> (Int,Int) -> (Int,Int) ->  Environment 
updateEnvironment envi "Robot" i f =  let i1 = fst i  
                                          j1 = snd i 
                                       in  
                                          let new_robots =  (dropElemList (robots envi) i1 j1 [] ) ++ [(Agent "Robot" (fst f) (snd f) )] 
                                          in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ new_robots, (children envi) ,(corrals envi),(obstacles envi ),(dirt envi)]) (holdChild envi)

updateEnvironment envi "Child" i f = let  i1 = fst i ; j1 = snd i in  let new_children =  (dropElemList (children envi) i1 j1 [] ) ++ [Agent "Child" (fst f) (snd f) ] 
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, new_children ,corrals envi,obstacles envi ,dirt envi]) (holdChild envi)
updateEnvironment envi "Corral" i f = let  i1 = fst i ; j1 = snd i in  let new_corrals = (dropElemList (corrals envi) i1 j1 [] ) ++ [Agent "Corral" (fst f) (snd f) ] 
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, children envi ,new_corrals,obstacles envi ,dirt envi]) (holdChild envi)
updateEnvironment envi "Obstacle" i f = let  i1 = fst i ; j1 = snd i in let new_obstacles =  (dropElemList (obstacles envi) i1 j1 [] ) ++ [Agent "Obstacle" (fst f) (snd f) ]
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, children envi ,corrals envi, new_obstacles  ,dirt envi]) (holdChild envi)
updateEnvironment envi "Dirt" i f =  let  i1 = fst i ; j1 = snd i in  let new_dirt =  (dropElemList (dirt envi) i1 j1 [] ) ++ [Agent "Dirt" (fst f) (snd f) ] 
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, children envi ,corrals envi, obstacles envi ,new_dirt]) (holdChild envi)
updateEnvironment envi "LoadedChild" i f =  let  i1 = fst i ; j1 = snd i in  let new_hold =  (dropElemList (holdChild envi) i1 j1 [] ) ++ [ Agent "Robot" (fst f) (snd f), Agent "Child" (fst f) (snd f)] 
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, children envi ,corrals envi, obstacles envi ,dirt envi]) (new_hold)                                        

createDirt:: Environment -> (Int,Int) ->  Environment 
createDirt envi i =  createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, children envi ,corrals envi, obstacles envi ,(dirt envi) ++ [Agent "Dirt" (fst i) (snd i) ] ]) (holdChild envi)    

main :: IO ()
main  =   do 
        let g = mkStdGen 50  
        let t = 1
        let t_final = 10
        let number_rows = 5
        let number_columns = 4
        let robots = [Agent "Robot" 1 0, Agent "Robot" 1 2] 
        let children = [Agent "Child" 0 1]
        let corrals = [Agent "Corral" 3 0]
        let obstacles = [Agent "Obstacle" 1 1, Agent "Obstacle" 2 1]
        let dirt = [] 
        let agents = [robots, children, corrals,  obstacles, dirt]
        let init_envi = createInitialEnvironment number_rows number_columns t t_final agents []
        let pirnt_in = printTable init_envi  number_columns number_rows 0 0 "" 
        let new_state = simulation init_envi g 
          in do 
            print(pirnt_in)
            print("Dimensions: " ++  show number_rows ++ "x" ++ show number_columns )
            print(new_state)

simulation :: Environment ->  StdGen -> Environment
simulation envi  g =  do 
                        let envi_robots =  alterRobots envi (robots envi)
                        let t_final = (t_Final envi_robots) - 1
                        let t = (t_Rnd envi_robots) - 1
                        let (envi_children,g1) = alterChildren envi_robots (children envi_robots ) g
                        let new_envi = changeTimeEnvironment envi_children t t_final 
                         in if t_Final new_envi  > 0 then simulation (printTable new_envi (number_Columns new_envi) (number_Rows new_envi) 0 0 "")  g1
                             else printTable new_envi (number_Columns new_envi) (number_Rows new_envi) 0 0 "" 

alterRobots :: Environment ->  [Agent] -> Environment
alterRobots envi []  =  envi 
alterRobots envi (robot:xs)  = let actions = generateRobotsActions envi (row robot) (column robot) 
                                   new_env = traceShow ((( "Acciones del Robot:",((row robot) ,(column robot))),actions)) (executeRobotActions envi (row robot) (column robot) actions ) in alterRobots new_env xs  
                          
alterChildren :: Environment -> [Agent] -> StdGen -> (Environment,StdGen)
alterChildren envi [] gen  = (envi,gen)
alterChildren envi (child:xs) gen = let (g1 , actions) =  generateChildActions envi (row child) (column child) gen 
                                            in  let (new_env,g2) = traceShow ((( "Acciones del Nene:",((row child) ,(column child))),actions))  ( executeChildActions envi (row child) (column child) actions g1 )
                                            in alterChildren new_env xs g2  

--Generate child actions                                                     
generateChildActions :: Environment-> Int -> Int -> StdGen -> (StdGen ,[Action])  
generateChildActions  envi row column gen = if elementBelongs (corrals envi) row column || haveLoadedChild envi  (Agent "Child" row column) then (gen ,[]) 
                                            else let (move_g, mov) = generateMoveActionChild envi row column gen 
                                                     dirty = generateDirtyAction 
                                                     in (move_g ,mov : [dirty])

   
generateMoveActionChild :: Environment -> Int -> Int -> StdGen -> (StdGen ,Action)
generateMoveActionChild envi row column gen =  let (g, action ) = generateSquaresMoveChild envi row column gen
                                                in ( g, Action "move" action ) 

generateDirtyAction :: Action
generateDirtyAction = Action "dirty" (-1,-1)

-- genera las casillas a las que puede moverse el nene 
generateSquaresMoveChild :: Environment -> Int -> Int -> StdGen -> (StdGen,(Int,Int))
generateSquaresMoveChild envi i j gen = let x1  = (i - 1 , j ) 
                                            x2 = (i , j + 1)      
                                            x3 = (i + 1, j )
                                            x4 = ( i , j - 1 ) 
                                           in let list  =  verifySquareMoveChild envi [x1,x2,x3,x4] []                   
                                                  ind = (length list)
                                                  in if ind > 0
                                                     then let (m,g) = randomR (0, ind) gen 
                                                            --in (g , list!!m) 
                                                            in (gen,(1,1))
                                                      else let x = (gen,(i,j)) 
                                                               in x
                                                

-- verifica q en esa casilla no hay ni suciedad ni robot ni corrall 
verifySquareMoveChild :: Environment -> [(Int,Int)]-> [(Int,Int)]->  [(Int,Int)]
verifySquareMoveChild envi [] result  = result 
verifySquareMoveChild envi (x:xs) result = let (i,j) = x 
                                                in  if verifyInBoard envi i j && (verifyIsEmpty envi i j || elementBelongs (obstacles envi) i j )
                                                    then  verifySquareMoveChild envi xs  (result ++ [x]) 
                                                    else verifySquareMoveChild envi xs result         
--fin

--Run Child Actions
executeChildActions :: Environment -> Int -> Int ->  [ Action ] -> StdGen -> (Environment,StdGen)
executeChildActions envi i j [] gen = (envi,gen)
executeChildActions envi i j  actions gen = if length actions == 2  
                                            then  let  (i_f,j_f) = position (actions!!0)
                                                       new_envi_m = executeChildMove envi i j (actions!!0)    
                                                   in executeChildDirt new_envi_m (i,j) (i_f,j_f) gen     
                                            else executeChildDirt envi (i,j) (i,j) gen

executeChildMove :: Environment -> Int -> Int ->  Action -> Environment
executeChildMove envi i j  action  =  let (i_f,j_f) = position action in if not (verifyIsEmpty envi i_f j_f ) 
                                        then let  (s,r) = (i_f + (i_f - i),j_f + (j_f -j))
                                                in let list = checkIfOffset envi (i_f,j_f) (s,r) [(i,j), (i_f,j_f)]
                                                     in let new_list = reverse list in executeChildMove1 envi new_list 
                                        else  updateEnvironment envi "Child" (i,j) (i_f,j_f)    

executeChildMove1 :: Environment -> [(Int,Int)] -> Environment
executeChildMove1 envi [] = envi
executeChildMove1 envi  (x :[]) = envi 
executeChildMove1 envi  (x:y:xs)  = if elementBelongs (children envi) (fst y) (snd y) 
                                   then   let new_envi = updateEnvironment envi "Child" y x in executeChildMove1 new_envi ([y] ++ xs)
                                   else  let newenvi = updateEnvironment envi "Obstacle" y x in executeChildMove1 newenvi  ([y] ++ xs)

checkIfOffset:: Environment -> (Int,Int)  -> (Int, Int) -> [(Int,Int)]-> [(Int,Int)]
checkIfOffset envi i f result =  let (i1,j1) = i ; (i2,j2) = f  in  if verifyInBoard envi i2 j2  &&  verifyIsEmpty envi i2 j2 
                                                                   then  result ++ [(i2,j2)] 
                                                                   else  if not (verifyInBoard envi i2 j2 ) || not (elementBelongs (obstacles envi) i2 j2 ) 
                                                                         then []
                                                                         else let i3 = i2 + (i2 - i1)
                                                                                  j3 = j2 + (j2 - j1)
                                                                               in checkIfOffset envi (i2,j2) (i3,j3) (result ++ [(i2,j2)]) 

executeChildDirt:: Environment -> (Int,Int) -> (Int,Int) -> StdGen -> (Environment,StdGen)
executeChildDirt envi  pos_i pos_f gen = let (i,j) = pos_i 
                                             (i_f , j_f) = pos_f
                                             list = generateSquare3x3 i j
                                             empty =  returnEmptyIn3x3 envi (i_f , j_f) list  [] 
                                             count_children = traceShow (empty) countChildrenIn3x3 envi list 0
                                             in if (length empty) > 0
                                                then let (m,g2) = randomR (0, (length empty) - 1 ) gen 
                                                         s =  createDirt envi (empty!!m) 
                                                         in (s,g2)
                                                else (envi,gen)         

--Generate robots actions 
generateRobotsActions:: Environment -> Int -> Int -> [Action] 
generateRobotsActions envi row column  =  let robot = Agent "Robot" row column 
                                              ways = initialBFS envi row column
                                               in if haveLoadedChild envi robot
                                                 then let ways_to_corral = takeChildToCorral envi ways
                                                          min1 = minimalPath ways_to_corral  (-1) (ways_to_corral!!0)
                                                          m = snd min1
                                                          in if length m == 3 then [ Action "move" ( m!!1 ) ,Action "move" (m!!2) ,Action "leave child" (m!!2) ]
                                                             else if length m == 2    then [ Action "move" (m!!1), Action "leave child" (m!!1)]
                                                                  else if length m == 1 then [ Action "leave child" (m!!0)]
                                                                       else [ Action "move" (m!!1), Action "move" (m!!2)]
                                                 else if isLooseChildBool envi (children envi) 
                                                      then  let list = isLooseChild envi (children envi)
                                                                ways_to_children =  takeChildren envi list ways
                                                                min2 =  minimalPath ways_to_children (-1) (ways_to_children!!0)
                                                                m1 = snd min2
                                                             in if length m1 == 2 then [ Action "move" (m1!!1),Action "pick up child" (m1!!1) ]
                                                               else if length m1 > 2    then [ Action "move" (m1!!1)]
                                                                  else [ Action "pick up child" (m1!!0) ]
                                                      else if thereIsDirt envi 
                                                           then let list1 = takeDirty ways 
                                                                    min3 = minimalPath list1 (-1) (list1!!0)
                                                                    m2 = snd min3
                                                                   in  if length m2 > 1 then [ Action "move" (m2!!1)]
                                                                       else [ Action "clean up" (m2!!0) ]
                                                            else [Action "move" (row,column)]           


--Execute Robots Actions 
executeRobotActions :: Environment -> Int -> Int ->  [Action ] -> Environment
executeRobotActions envi row column [] = envi
executeRobotActions envi row column (action:xs) = case (name action) of  "move" -> let  (new ,i,j)  =  executeRobotMove envi row column action in  executeRobotActions new i j xs
                                                                         "clean up"-> let new =  executeRobotCleanUp envi row column action in executeRobotActions new row column xs   
                                                                         "pick up child" -> let new = executeRobotPickUpChild envi row column action in executeRobotActions new row column xs
                                                                         "leave child"-> let new =  executeRobotLeaveChild envi row column action in executeRobotActions new row column xs



executeRobotMove ::  Environment -> Int -> Int -> Action -> (Environment,Int,Int)   
executeRobotMove envi i j action = let (i_f,j_f) = (position action) in if elementBelongs (robots envi) i_f j_f 
                                                                        then (envi,i,j) 
                                                                        else  if haveLoadedChild envi  (Agent "Robot" i j) 
                                                                              then let new = updateEnvironment envi "Robot" (i,j) (i_f,j_f)
                                                                                          in  let ret = updateEnvironment new "Child" (i,j) (i_f,j_f) 
                                                                                                    in let s = updateEnvironment ret "LoadedChild" (i,j) (i_f,j_f) in (s, i_f, j_f)
                                                                        else let s = updateEnvironment envi "Robot" (i,j) (i_f,j_f) in (s,i_f, j_f)
executeRobotLeaveChild ::  Environment -> Int -> Int -> Action -> Environment  
executeRobotLeaveChild envi i j action = let (i_f,j_f) = (position action) ; new = dropElemList (holdChild envi) i_f j_f []; 
                                         in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) [(robots envi) , (children envi) ,(corrals envi) , (obstacles envi) ,(dirt envi) ] new 

f  envi i j =  (envi,i,j)
executeRobotCleanUp :: Environment -> Int -> Int -> Action -> Environment 
executeRobotCleanUp envi i j action = let (i_f,j_f) = (position action); s = dropElemList(dirt envi) i_f j_f []
                                      in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) [(robots envi) ,(children envi) ,(corrals envi) ,(obstacles envi) ,  s ] (holdChild envi) 

executeRobotPickUpChild::Environment -> Int -> Int -> Action -> Environment                                                                        
executeRobotPickUpChild envi i j action = let (i_f,j_f) = (position action);new =  (holdChild envi) ++ [Agent "Robot" i_f j_f , Agent "Child" i_f j_f ]; s = [(robots envi) , (children envi) ,(corrals envi), (obstacles envi) , (dirt envi) ]
                                           in if (not (haveLoadedChild envi  (Agent "Child" i_f j_f) ))&& ((Agent "Child" i_f j_f)  `elem` (children envi))
                                               then createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) s new 
                                               else envi
-- Buscar los caminos 
initialBFS ::Environment -> Int -> Int -> [(Agent,[(Int,Int)])]  
initialBFS envi i j = bfs envi  i j [((i,j),[(i,j)])] [(i,j)] []

bfs :: Environment -> Int -> Int -> [((Int,Int),[(Int,Int)])] -> [(Int,Int)]-> [(Agent,[(Int,Int)])]-> [(Agent,[(Int,Int)])]
bfs envi row column [] visited result = result 
bfs envi row column (p:pending) visited result = let (x, way) = p
                                                     i = fst x 
                                                     j = snd x
                                                     agents = returnAgent envi i j
                                                     new = construct agents way 
                                                     new_result =  result ++  new 
                                                    in  if validateAdjacent envi row column i j
                                                         then let (new_visited, new_pending) = expand envi i j way visited pending  
                                                               in bfs envi row column  new_pending new_visited new_result
                                                         else bfs envi row column pending visited new_result     
                                                            

expand :: Environment -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [((Int,Int),[(Int,Int)])] -> ([(Int, Int)],[((Int,Int),[(Int,Int)])])
expand envi i j way visited pending  =  let adjacents = generateAdjacents i j
                                            y = inBoard envi adjacents
                                            (new_list, new_visited ) = isVisited visited y []
                                            new_pending = addAdjacentPending new_list way pending 
                                            in ( new_visited, new_pending)                              

--Verificar si ciertas posiciones estan visitadas.

isVisited:: [(Int,Int)] -> [(Int,Int)]-> [(Int,Int)] ->( [(Int,Int)],[(Int,Int)])
isVisited visited [] result =  (result, visited) 
isVisited visited (adj: xs) result  = if adj `elem` visited
                              then isVisited visited xs result 
                              else isVisited (visited ++ [adj]) xs (result ++ [adj])

--Agregar a los adyacentes a pendintes con el camino generado hasta el momento 
addAdjacentPending :: [(Int, Int)] -> [(Int, Int)] -> [((Int,Int),[(Int,Int)])] -> [((Int,Int),[(Int,Int)])] 
addAdjacentPending  []      way  pending  = pending
addAdjacentPending (adj: xs) way  pending =  let new = (adj, way ++ [adj]) 
                                              in  addAdjacentPending xs way (pending ++ [new])                                           

generateAdjacents :: Int -> Int -> [(Int,Int)]
generateAdjacents i j = let x1  = (i - 1 , j ) 
                            x2 = (i , j + 1)      
                            x3 = (i + 1, j )
                            x4= ( i , j - 1 )
                            in [x1,x2,x3,x4] 

--Dado una lista de posiciones devuelve las que estan en los limites del tablero
inBoard :: Environment -> [(Int,Int)] -> [(Int,Int)] 
inBoard envi [] = []
inBoard envi (x:xs) =  if  verifyInBoard envi (fst x) (snd x)  then [x] ++ inBoard envi xs  else  inBoard envi xs                       

--validar si una posicion puede ser expandida 
validateAdjacent:: Environment -> Int -> Int -> Int -> Int -> Bool
validateAdjacent envi row column i j | row == i && column == j = True 
                                     | verifyIsEmpty envi i j = True
                                     | not ( elementBelongs (robots envi) i j ) && (elementBelongs (corrals envi) i j || elementBelongs (children envi) i j)= True
                                     | otherwise = False
      
construct :: [Agent] -> [(Int,Int)] -> [(Agent,[(Int,Int)] )]
construct [] list = []
construct (x : xs) list  = (x,list) : construct xs list

--Retorna los agentes de interes que estan en una posicion 
returnAgent:: Environment -> Int -> Int -> [Agent]
returnAgent envi i j =  let item =  [ x | x  <- children envi ,row x == i , column x == j] 
                            item1 = [ x | x  <- corrals envi ,row x == i , column x == j]
                            item2 = [ x | x  <- dirt envi ,row x == i , column x == j ]
                            in item1 ++ item2 ++ item    
                            
--Devuelve la lista de los nenes sueltos
isLooseChild :: Environment -> [Agent] -> [Agent]
isLooseChild envi [] = []
isLooseChild envi (child : xs)  =  if not (elementBelongs (corrals envi) (row child) (column child)) && not (haveLoadedChild envi (Agent "Child" (row child) (column child) ))
                                  then [child] ++  isLooseChild envi  xs
                                  else 
                                     isLooseChild envi  xs

-- Si hay nene suelto
isLooseChildBool :: Environment -> [Agent] ->  Bool
isLooseChildBool envi children  =  let list = isLooseChild envi children in if length list > 0 then True else False 

--Saber si un robot tiene un nene cargado o el nene esta cargado 
haveLoadedChild:: Environment -> Agent ->  Bool
haveLoadedChild envi agent = agent `elem` (holdChild envi) 

-- Saber si hay suciedad 
thereIsDirt:: Environment -> Bool 
thereIsDirt envi = length (dirt envi) > 0  

togetherInSquare :: Environment -> Int -> Int -> Bool  
togetherInSquare  envi i j = elementBelongs (children envi) i j ||  elementBelongs (corrals envi) i j ||  elementBelongs (dirt envi) i j

--Utiles de generara acciones de robot 
takeChildToCorral:: Environment ->  [(Agent,[(Int,Int)])] -> [(Agent,[(Int,Int)])]
takeChildToCorral envi []  = []
takeChildToCorral envi ( x : xs) = if (type_ (fst x) == "Corral") &&  not (elementBelongs (children envi) (row (fst x)) (column (fst x))) && not (elementBelongs (robots envi) (row (fst x)) (column (fst x)))
                                       then x: takeChildToCorral envi xs 
                                       else takeChildToCorral envi xs
--Buscar nene suelto
takeChildren :: Environment -> [Agent] -> [(Agent,[(Int,Int)])] -> [(Agent,[(Int,Int)])]
takeChildren envi []  list = []
takeChildren envi (children_out:xs)  list  = let list1 = takeChildren1 envi children_out list  in list1 ++ takeChildren envi xs list

takeChildren1 :: Environment -> Agent -> [(Agent,[(Int,Int)])] -> [(Agent,[(Int,Int)])]
takeChildren1 envi agent  [] = []
takeChildren1 envi agent (item:xs) = if agent == (fst item) then item: takeChildren1 envi agent  xs  else takeChildren1 envi agent  xs 

--Busca las suciedades
takeDirty :: [(Agent,[(Int,Int)])] -> [(Agent,[(Int,Int)])]
takeDirty []    = []
takeDirty (item:xs) = let agent = (fst item) in if type_ agent == "Dirt" then item : takeDirty xs else takeDirty xs

-- De caminos a agentes del mismo tipo devuelve el min                                             
minimalPath :: [(Agent,[(Int,Int)])] -> Int ->  (Agent,[(Int,Int)]) -> (Agent,[(Int,Int)]) 
minimalPath [] min result = result
minimalPath (x:xs) (-1) result  = let min1 = length (snd x) in minimalPath xs min1  x
minimalPath (x:xs) min_ result = let min1 = length (snd x) in if min1 < min_ then  minimalPath xs min1 x
                                                                            else   minimalPath xs min_ result 


                            
                            
