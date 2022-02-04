module Board where
import Structure 
import Debug.Trace

printTable ::Environment -> Int -> Int -> Int -> Int -> String -> Environment 
printTable env  number_columns number_rows i j string | i == number_rows && j == number_columns =  env   
                                                          | j == number_columns =  trace(string ++ "|") (printTable env number_columns number_rows (i+1) 0 "")
                                                          | (length [ x | x <- robots env, row x == i && column x == j  ] > 0) && (length [ x| x <- children env, row x == i && column x == j  ] > 0) && (length [ x| x <- corrals env, row x == i && column x == j  ] > 0) = let str = string ++ "|CRN" in printTable env number_columns number_rows i (j+1)  str 
                                                          | (length [ x | x <- robots env, row x == i && column x == j  ] > 0) && (length [ x| x <- children env, row x == i && column x == j  ] > 0) = let str = string ++ "|RN " in printTable env number_columns number_rows i (j+1)  str 
                                                          | length [ x | x <- robots env, row x == i && column x == j  ] > 0 = let str = string ++ "| R " in printTable env number_columns number_rows i (j+1)  str 
                                                          | length [ x | x <- children env, row x == i && column x == j  ] > 0  && length [ x| x <- corrals env, row x == i && column x == j  ] > 0 = let str = string ++ "|NC " in printTable env  number_columns number_rows i (j+1)  str 
                                                          | length [ x | x <- children env, row x == i && column x == j  ] > 0  =let str = string ++ "| N " in printTable env  number_columns number_rows i (j+1)  str 
                                                          | length [ x | x <- corrals env, row x == i && column x == j  ] > 0 = let str = string ++ "| C " in printTable env  number_columns number_rows i (j+1) str
                                                          | length [ x | x <- obstacles env, row x == i && column x == j  ] > 0  = let str = string ++ "| O " in printTable env  number_columns number_rows i (j+1)  str 
                                                          | length [ x | x <- dirt env, row x == i && column x == j  ] > 0  = let str = string ++ "| D " in printTable env  number_columns number_rows i (j+1)  str
                                                          | otherwise = let str = string ++ "|   " in printTable env  number_columns number_rows i (j+1)  str                             
    
    