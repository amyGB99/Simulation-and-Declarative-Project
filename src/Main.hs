module Main where       
import Debug.Trace
import Data.List
import System.Random 

--Data Structure :
data Agent = Agent { row :: Int, column:: Int }deriving (Show)

data Environment = Environment {
number_Rows :: Int,
number_Columns :: Int,
robots :: [Agent],
children :: [Agent],
obstacles :: [Agent],
corrals :: [Agent],
dirt :: [Agent],
t_Rnd :: Int,
t_Final:: Int }deriving (Show)

main :: IO ()
main  =   do 
        let g = mkStdGen 10  
        let t = 5
        let t_final = 2
        let number_columns = 10
        let number_rows = 10
        let count_children = 2 
        let count_robots  = 4 
        let max_agents = ((number_columns * number_rows) `div`  2 )
        let total = 2 *count_children +  count_robots 
        let max =  max_agents - total 
        let count_dirt = 2
        let count_obstacles = 2
        if max > 0 
        then          
         let total = 2 * count_children +  count_robots 
             --Robots
             list = createAgents count_robots number_rows number_columns g
             robots = [ fst x |x <- list]
             generators = [ snd x | x <- list] 
             g1 = generators!! ((length generators) - 1)
             -- Children
             list1 = createAgents count_children number_rows number_columns g1
             children = [ fst x |x <- list1 ] 
             generators1 = [ snd x | x <- list1] 
             g2 = generators1!! ((length generators1) - 1)
             --Corrals
             list2 = createAgents count_children number_rows number_columns g2
             corrals = [ fst x |x <- list2 ] 
             generators2 = [ snd x | x <- list2] 
             g3 = generators2!! ((length generators2) - 1)
             --Dirt
             list3 = createAgents count_dirt number_rows number_columns g3
             dirt = [ fst x |x <- list3 ] 
             generators3 = [ snd x | x <- list3] 
             g4 = generators3!! ((length generators3) - 1)
             --Obstacles
             list4 = createAgents count_obstacles number_rows number_columns g4
             obstacles = [ fst x |x <- list4 ] 
             generators4 = [ snd x | x <- list4] 
             g5 = generators4!! ((length generators4) - 1)
             --Environment
             agents = [robots, children, corrals, dirt, obstacles]
             init_envi = createInitialEnvironment number_rows number_columns t t_final agents
             new_state = simulation init_envi g5 
         in  do 
                print("Dimensions: " ++  show number_rows ++ "x" ++ show number_columns )
                print(new_state)
        else 
         print False     
        print True       

-- Create Data
createAgent ::Int -> Int -> StdGen -> (Agent,StdGen)
createAgent  number_rows number_columns g = let (i,g1) = randomR (0,number_rows-1) g  
                                                (j,g2) = randomR (0,number_columns - 1) g1
                                                     in (Agent i j, g2)   

createAgents ::  Int ->Int -> Int -> StdGen -> [(Agent,StdGen)]
createAgents 0 number_rows number_columns g = []
createAgents count number_rows number_columns g = let (agent,g1) = createAgent number_rows number_columns g 
                                                         in [(agent,g1)] ++ createAgents (count-1) number_rows number_columns g1
                                                         
createInitialEnvironment :: Int -> Int -> Int -> Int -> [[Agent]]  ->  Environment
createInitialEnvironment number_rows number_columns t t_final agentss  =  Environment { robots = agentss!!0,
                                                                                        children = agentss!!1,
                                                                                        obstacles =  agentss!!2,
                                                                                        corrals = agentss!!3,
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


duplicate:: [String] -> Int -> [String]
duplicate string count = if count > 0
                         then string ++  duplicate string (count-1)
                         else []

alterAgents :: Environment -> Environment
alterAgents envi = let  envi_robots =  alterRobots  envi (length (robots envi)- 1 ) 
                   in alterChildren envi_robots  (length ( children envi) - 1 )
                        
                                       
alterRobots :: Environment -> Int ->  Environment
alterRobots envi (-1) = envi
alterRobots envi count  = traceShow ((robots envi)!!count) (alterRobots  envi (count - 1) ) 

alterChildren :: Environment -> Int  -> Environment
alterChildren envi (-1) = envi
alterChildren envi count  = traceShow ((children envi)!!count) (alterChildren envi (count - 1))

alterRndEnvironment :: Environment ->  Environment
alterRndEnvironment envi   =  envi

simulation :: Environment ->  StdGen -> Environment
simulation envi  g =  if t_Final envi  > 0 
                                then  do 
                                        let envi_agent = alterAgents envi 
                                        let envi_rnd = alterRndEnvironment envi_agent 
                                        let t = (t_Final envi) - 1
                                        let t_final = (t_Rnd envi) - 1
                                        let new_envi = changeTimeEnvironment envi_rnd t t_final
                                             in simulation (printTable new_envi (number_Columns new_envi) (number_Rows new_envi) 0 0 "")  g
                                else envi

printTable ::Environment -> Int -> Int -> Int -> Int -> String -> Environment 
printTable env  number_columns number_rows i j string | i == number_rows && j == number_columns =  env   
                                                                | j == number_columns =  trace(string ++ "|") (printTable env number_columns number_rows (i+1) 0 "")
                                                                | length ([ x| x <- robots env, row x == i && column x == j  ]) > 0 = let str = string ++ "| R " in (printTable env number_columns number_rows i (j+1)  str )
                                                                | length ([ x| x <- children env, row x == i && column x == j  ]) > 0  =let str = string ++ "| N " in printTable env  number_columns number_rows i (j+1)  str 
                                                                | length ([ x| x <- corrals env, row x == i && column x == j  ]) > 0 = let str = string ++ "| C " in printTable env  number_columns number_rows i (j+1) str
                                                                | length ([ x| x <- obstacles env, row x == i && column x == j  ]) > 0  = let str = string ++ "| O " in printTable env  number_columns number_rows i (j+1)  str 
                                                                | length ([ x| x <- dirt env, row x == i && column x == j  ]) > 0  = let str = string ++ "| D " in printTable env  number_columns number_rows i (j+1)  str
                                                                |otherwise = let str = string ++ "|   " in printTable env  number_columns number_rows i (j+1)  str                             



