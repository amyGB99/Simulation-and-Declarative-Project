module Structure where 
import System.Random
    

--Data Structure :
data Agent = Agent { row :: Int, column:: Int }deriving (Show,Eq)

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

data Action = Action { name :: String ,position :: (Int,Int)} deriving (Show)     

