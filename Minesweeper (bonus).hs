type Cell =(Int, Int)
 
data MyState = Null |S Cell [Cell] String MyState [Cell] deriving (Show,Eq)

up ( S (i,j) z str state v ) |i==0 =Null
							 |elem (i-1,j) v = Null
							 |collect(  S (i-1,j) z "up"  ( S (i,j) z str state v ) v ) /= Null = collect( S (i-1,j) z "up"  ( S (i,j) z str state v) (v++ [(i-1,j)]) ) 
						     |otherwise =  S (i-1,j) z "up"  ( S (i,j) z str state (v)) (v++[(i-1,j)]) 

down ( S (i,j) z str state v)|i==5 =Null
							 | elem (i+1,j) v = Null
						     |collect(  S (i+1,j) z "down"  ( S (i,j) z str state v) v ) /= Null = collect( S (i+1,j) z "down"  ( S (i,j) z str state v) (v++ [(i+1,j)]) ) 
						     |otherwise =  S (i+1,j) z "down"  ( S (i,j) z str state (v)) (v++[(i+1,j)])

right ( S (i,j) z str state v)|j==5 =Null
							  | elem (i,j+1) v = Null
                              |collect(  S (i,j+1) z "right"  ( S (i,j) z str state v ) v ) /= Null = collect( S (i,j+1) z "right"  ( S (i,j) z str state v) (v++ [(i,j+1)]) ) 
						      |otherwise =  S (i,j+1) z "right"  ( S (i,j) z str state (v)) (v++[(i,j+1)])

left ( S (i,j) z str state v) |j==0 =Null
							  |elem (i,j-1) v = Null
                              |collect(  S (i,j-1) z "left"  ( S (i,j) z str state v ) v ) /= Null = collect( S (i,j-1) z "left"  ( S (i,j) z str state v) (v++ [(i,j-1)]) ) 
						      |otherwise =  S (i,j-1) z "left"  ( S (i,j) z str state (v))(v++[(i,j-1)])
delete _ [] = []
delete y (x:xs)| y==x = delete y xs
			   | otherwise = (x:delete y xs)
			   
			   
collect  (S position z str state  v)  
	|(elem position z) = ( S position (delete position z) "collect" ( S position z str state v ) v )
	|otherwise = Null


nextMyStates (S position z str state  v)  = delete Null [(up (S position z str state  v) ), 
											(down (S position z str state  v) ), 
											(left (S position z str state  v) ), 
											(right (S position z str state  v) )]
											


isGoal ( S _ z _ _ _ ) = (z==[])

search (x:xs) | isGoal(x) = x
			  | otherwise = search (xs++nextMyStates(x))

constructSolution ( S _ _ "" Null  _) = []
constructSolution ( S _ _ reqstr state _ ) = constructSolution state ++ [reqstr] 


solve c mines= constructSolution (search [(S c mines "" Null [])] )



--solve (0,0) [(0,1)]


















