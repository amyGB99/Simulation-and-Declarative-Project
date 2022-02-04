module Main where       
import Debug.Trace
import Data.List
import System.Random 
import Structure
import Board
import Utils
       

-- Create Data
createAgent ::Int -> Int -> StdGen -> (Agent,StdGen)
createAgent  number_rows number_columns g = let (i,g1) = randomR (0,number_rows-1) g  
                                                (j,g2) = randomR (0,number_columns - 1) g1
                                                     in (Agent i j, g2)   

createAgents ::  Int ->Int -> Int -> StdGen -> [(Agent,StdGen)]
createAgents 0 number_rows number_columns g = []
createAgents count number_rows number_columns g = let (agent,g1) = createAgent number_rows number_columns g 
                                                         in [(agent,g1)] ++ createAgents (count - 1) number_rows number_columns g1
                                                         
createInitialEnvironment :: Int -> Int -> Int -> Int -> [[Agent]]  ->  Environment
createInitialEnvironment number_rows number_columns t t_final agentss  =  Environment { robots = agentss!!0,
                                                                                        children = agentss!!1,
                                                                                        obstacles =  agentss!!3,
                                                                                        corrals = agentss!!2,
                                                                                        dirt = agentss!!4,
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
                                                    number_Rows =  number_Rows envi ,
                                                    number_Columns = number_Columns envi,
                                                    t_Rnd = t,
                                                    t_Final = t_final}

updateEnvironment :: Environment -> String -> (Int,Int) -> (Int,Int) ->  Environment 
updateEnvironment envi "Robot" i f =  let  i1 = fst i ; j1 = snd i in  let new_robots =  (dropElemList (robots envi) i1 j1 [] ) ++ [Agent (fst f) (snd f) ] 
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ new_robots, children envi ,corrals envi,obstacles envi ,dirt envi])
updateEnvironment envi "Child" i f = let  i1 = fst i ; j1 = snd i in  let new_children =  (dropElemList (children envi) i1 j1 [] ) ++ [Agent (fst f) (snd f) ] 
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, new_children ,corrals envi,obstacles envi ,dirt envi])
updateEnvironment envi "Corral" i f = let  i1 = fst i ; j1 = snd i in  let new_corrals = (dropElemList (corrals envi) i1 j1 [] ) ++ [Agent (fst f) (snd f) ] 
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, children envi ,new_corrals,obstacles envi ,dirt envi])
updateEnvironment envi "Obstacle" i f = let  i1 = fst i ; j1 = snd i in let new_obstacles =  (dropElemList (obstacles envi) i1 j1 [] ) ++ [Agent (fst f) (snd f) ]
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, children envi ,corrals envi, new_obstacles  ,dirt envi])
updateEnvironment envi "Dirt" i f =  let  i1 = fst i ; j1 = snd i in  let new_dirt =  (dropElemList (dirt envi) i1 j1 [] ) ++ [Agent (fst f) (snd f) ] 
                                        in createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, children envi ,corrals envi, obstacles envi ,new_dirt])

createDirt:: Environment -> (Int,Int) ->  Environment 
createDirt envi i =  createInitialEnvironment (number_Rows envi) (number_Columns envi) (t_Rnd envi) (t_Final envi) ([ robots envi, children envi ,corrals envi, obstacles envi ,(dirt envi) ++ [Agent (fst i) (snd i) ] ])     
--fin!


--main :: IO ()
--main  =   do 
--let g = mkStdGen 50  
      --  let t = 5
      --  let t_final = 1
       -- let number_columns = 5
      --  let number_rows = 4
      --  let count_children = 1 
       -- let count_robots  = 1 
      --  let max_agents = ((number_columns * number_rows) `div`  2 )
       -- let total = 2 *count_children +  count_robots 
       -- let max =  max_agents - total 
       -- let count_dirt = 1
       -- let count_obstacles = 2
       -- if max > 0 
       -- then          
       --  let total = 2 * count_children +  count_robots 
             --Robots
             --list = createAgents count_robots number_rows number_columns g
             --robots = [ fst x |x <- list]
             --generators = [ snd x | x <- list] 
             --g1 = generators!! ((length generators) - 1)
             -- Children
             --list1 = createAgents count_children number_rows number_columns g1
             --children = [ fst x |x <- list1 ] 
             --generators1 = [ snd x | x <- list1] 
             --g2 = generators1!! ((length generators1) - 1)
             --Corrals
             --list2 = createAgents count_children number_rows number_columns g2
             --corrals = [ fst x |x <- list2 ] 
             --generators2 = [ snd x | x <- list2] 
             --g3 = generators2!! ((length generators2) - 1)
             --Dirt
             --list3 = createAgents count_dirt number_rows number_columns g3
             --dirt = [ fst x |x <- list3 ] 
             --generators3 = [ snd x | x <- list3] 
             --g4 = generators3!! ((length generators3) - 1)
             --Obstacles
             --list4 = createAgents count_obstacles number_rows number_columns g4
             --obstacles = [ fst x |x <- list4 ] 
             --generators4 = [ snd x | x <- list4] 
             --g5 = generators4!! ((length generators4) - 1)
             --Environment
             --agents = [robots, children, corrals,  obstacles, dirt]
             --init_envi = createInitialEnvironment number_rows number_columns t t_final agents
             --pirnt_in = printTable init_envi  number_columns number_rows 0 0 "" 
             --new_state = simulation init_envi g5 
        
         --in  do 
           --     print(init_envi)
             --   print(dirt)
               -- print("Dimensions: " ++  show number_rows ++ "x" ++ show number_columns )
                --print(new_state)
        --else 
         --print False     
        --print True       

main :: IO ()
main  =   do 
        let g = mkStdGen 50  
        let t = 5
        let t_final = 1
        let number_columns = 5
        let number_rows = 4
        let robots = [Agent 1 2 , Agent 2 4] 
        let children = [Agent 0 1]
        let corrals = []
        let obstacles = [Agent 1 1, Agent 2 1]
        let dirt = [Agent 0 4] 
        let agents = [robots, children, corrals,  obstacles, dirt]
        let init_envi = createInitialEnvironment number_rows number_columns t t_final agents
        let pirnt_in = printTable init_envi  number_columns number_rows 0 0 "" 
        let new_state = simulation init_envi g 
            in do 
                print(pirnt_in)
                print("Dimensions: " ++  show number_rows ++ "x" ++ show number_columns )
                print(new_state)

simulation :: Environment ->  StdGen -> Environment
simulation envi  g =  do 
                        let (envi_agent,g1) =  alterAgents envi g
                        let envi_rnd = alterRndEnvironment envi_agent 
                        let t_final = (t_Final envi) - 1
                        let t = (t_Rnd envi) - 1
                        let new_envi = changeTimeEnvironment envi_rnd t t_final
                        if t_Final new_envi  > 0 then simulation (printTable new_envi (number_Columns new_envi) (number_Rows new_envi) 0 0 "")  g1
                                else printTable new_envi (number_Columns new_envi) (number_Rows new_envi) 0 0 "" 
        
--Alter Enviroment 
--alterAgents :: Environment -> StdGen -> (Environment,StdGen)
--alterAgents envi g = let  (envi_robots, g1) =  alterRobots  envi (robots envi) g 
  --                 in alterChildren envi_robots (children envi_robots) g1
alterAgents :: Environment -> StdGen -> (Environment,StdGen)
alterAgents envi g =  alterChildren envi (children envi ) g                   
                                                             
alterRobots :: Environment ->  [Agent] -> StdGen -> (Environment,StdGen)
alterRobots envi [] gen = (envi,gen)
alterRobots envi (robot:xs) gen = traceShow (robot) (alterRobots envi xs gen) 

alterChildren :: Environment -> [Agent] -> StdGen -> (Environment,StdGen)
alterChildren envi [] gen  = (envi,gen)
alterChildren envi (child:xs) gen = let (g1 , actions) =  generateChildActions envi (row child) (column child) gen 
                                            in  let (new_env,g2) = traceShow (actions)( executeChildActions envi (row child) (column child) actions g1 )
                                            in alterChildren new_env xs g2  
                                           
alterRndEnvironment :: Environment ->  Environment
alterRndEnvironment envi   =  envi
-- fin


--Generate child actions                                                     
generateChildActions :: Environment-> Int -> Int -> StdGen -> (StdGen ,[Action])  
generateChildActions  envi row column gen = if elementBelongs (corrals envi) row column ||  elementBelongs (robots envi) row column then (gen ,[]) 
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
                                            x4= ( i , j - 1 ) 
                                           in let list  =  verifySquareMoveChild envi [x1,x2,x3,x4] []                   
                                                  ind = (length list)  - 1
                                                  (m,g2) =randomR (0, ind) gen 
                                                   --in (g2,list !! m)
                                                   in ( g2,(1,1))

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
                                        else updateEnvironment envi "Child" (i,j) (i_f,j_f)    

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
                                             empty = returnEmptyIn3x3 envi (i_f , j_f) list  [] 
                                             count_children = countChildrenIn3x3 envi list 0
                                             (m,g2) = randomR (0, (length empty) - 1 ) gen 
                                             s = traceShow(empty) (createDirt envi (empty!!m) )
                                             in (s,g2)
                                                
l envi i = (envi,[])
m envi = envi
