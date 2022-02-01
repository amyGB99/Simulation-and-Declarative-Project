{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

main :: IO ()
main  = let initial_env = createInitialEviroment 2 3 4 10 in let t = t_Rnd initial_env
                                                                 t_final =  t_Final initial_env 
                                                                 agentss = agents initial_env 
                                                                 in  print( simulation initial_env  t t_final agentss)  

data Agent = Agent { type_Agent :: String,  row :: Int, column:: Int }deriving (Show)

data Enviroment = Enviroment {agents :: [Agent],
number_Rows :: Int,
number_Columns :: Int,
t_Rnd :: Int,
t_Final:: Int }deriving (Show)

createAgent :: [String] -> [Int] -> [Int] -> [Agent]
createAgent [x] [y] [z] =  [Agent x y z]
createAgent (x:xs) (y:ys) (z:zs) = Agent { type_Agent = x , row = y, column = z} : createAgent xs ys zs


createAgents :: Int -> Int -> [Agent]
createAgents number_rows number_columns  =  createAgent  ["Child", "Obstacle", "Robot", "Dirt ", " Corral"] [0,2,3,4, 1] [4,3,2,0, 1]

createInitialEviroment :: Int -> Int -> Int -> Int -> Enviroment
createInitialEviroment number_rows number_columns t t_final = Enviroment { agents = createAgents number_rows  number_columns,
number_Rows =  number_rows ,
number_Columns = number_columns,
t_Rnd = t,
t_Final = t_final}

alterAgents :: Enviroment -> Int -> Int -> [Agent] -> Enviroment
alterAgents envi t t_final agents  = let envi_robots = alterRobots envi t t_final [ x | x <- agents, type_Agent x == "Robot "]
                                         envi_children = alterChildren  envi_robots   t t_final [ x | x <- agents, type_Agent x == "Children "]
                                         in envi_children 

alterRobots :: Enviroment -> Int -> Int -> [Agent] ->  Enviroment 
alterRobots envi t t_final robots  = envi

alterChildren :: Enviroment -> Int -> Int -> [Agent] -> Enviroment
alterChildren envi  t t_final children  = envi

alterRndEnviroment :: Enviroment -> Int -> Int -> Enviroment
alterRndEnviroment envi t t_final  =  envi

simulation :: Enviroment -> Int -> Int ->  [Agent]-> Enviroment
simulation envi t t_final agents =  let envi_Agents = alterAgents envi t t_final agents 
                                        envi_rnd = alterRndEnviroment envi_Agents t t_final  
                                        in if t_final  > 0 then let t1 = t - 1  
                                                                    t_final1 = t_final - 1 
                                        in simulation envi_rnd  t1 t_final1 agents else envi_rnd       
  

