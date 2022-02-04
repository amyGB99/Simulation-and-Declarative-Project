module Utils where
import Structure

--useful methods
--Vrerificar si una posicion esta dentro del limite del tablero 
verifyInBoard :: Environment ->  Int -> Int  -> Bool
verifyInBoard envi i j   = i >= 0 && i < number_Rows envi  && j >= 0 && j < number_Columns envi 

-- verificar que una poscion no este ocupada por ningun miembro 
verifyIsEmpty :: Environment->   Int -> Int  -> Bool
verifyIsEmpty env i j = not(elementBelongs (robots env) i j ) && 
                        not (elementBelongs (children env) i j ) && 
                        not (elementBelongs (obstacles env) i j )&& 
                        not (elementBelongs (corrals env) i j )&& 
                        not( elementBelongs (dirt env) i j )
                        
-- verificar si en esa posicion hay un agente de un tipo en especifico 
elementBelongs :: [Agent] -> Int -> Int -> Bool
elementBelongs agents i j  =  (i,j) `elem` [ (row x, column x ) | x <- agents ]

--eliminar un agente de una lista 
dropElemList :: [Agent]->  Int -> Int-> [Agent]-> [Agent]
dropElemList [] i j result = result
dropElemList (x:xs) i j result = if row x == i && column x == j
                                then  dropElemList xs i j result
                                else dropElemList xs i j (result ++ [x])
-- genera a partir de un i,j las casillas 3x3
generateSquare3x3:: Int -> Int -> [(Int,Int)]
generateSquare3x3 i j = let x1  = (i - 1 , j ) 
                            x2 = (i - 1 , j + 1)
                            x3 = (i , j + 1) 
                            x4 = (i + 1, j + 1)      
                            x5 = (i + 1, j )
                            x6 = (i + 1, j - 1) 
                            x7= ( i , j - 1 )
                            x8 = (i - 1 ,j - 1)  
                         in [x1,x2,x3,x4,x5,x6,x7,x8]   
                                  
--retorna en un rango 3x3 aquellas casillas que esten vacias y tambien verifica que no sea la casilla a donde se va a mover el nene
returnEmptyIn3x3 :: Environment -> (Int,Int) -> [(Int, Int)] -> [(Int, Int)]-> [(Int,Int)]
returnEmptyIn3x3 envi  pos [] result = result
returnEmptyIn3x3 envi  pos (x: xs) result =  let i = fst x 
                                                 j = snd x 
                                                  in if not (verifyIsEmpty envi i j ) ||  x == pos || not (verifyInBoard envi i j)
                                                    then  returnEmptyIn3x3 envi pos xs (result)
                                                    else returnEmptyIn3x3 envi  pos xs (result ++ [x])

--cantidad de ni;os en una casilla 3x3                                                    
countChildrenIn3x3 ::Environment -> [(Int, Int)]->  Int -> Int    
countChildrenIn3x3 envi [] count = count  
countChildrenIn3x3 envi  (x:xs) count = let i = fst x 
                                            j = snd x 
                                            in if length [m | m <- children envi ,  row m == i, column m == j] > 0
                                                    then countChildrenIn3x3 envi xs (count + 1)
                                                    else countChildrenIn3x3 envi xs count 
--fin
