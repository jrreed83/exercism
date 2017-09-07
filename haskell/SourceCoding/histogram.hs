module SourceCoding.Histogram (histogram) where

     data Tree a = E | B a (Tree a) (Tree a) deriving Show

     find :: (Eq a) => [(a,b)] -> a -> Maybe b 
     find []    _  = Nothing
     find ((key,val):t) x 
          | x == key   = Just val 
          | otherwise  = find t x

     update :: (Eq a) => [(a,Int)] -> a -> [(a,Int)] 
     update list x
	 = inner list x []
         where inner []        x front = front ++ [(x,1)] 
               inner ((k,v):t) x front
                    | x == k    = front ++ [(k,v+1)] ++ t
		    | otherwise = inner t x (front ++ [(k,v)]) 
    

     histogram :: (Eq a) => [a] -> [(a,Int)] 
     histogram list = foldl (\hist x -> update hist x) [] list

     tree_insert :: (Ord a) => Tree a -> a -> Tree a
     tree_insert E                x = B x E E
     tree_insert (B y left right) x 
	| x < y      = B y (tree_insert left x) right
	| otherwise  = B y left (tree_insert right x)

     list_to_bst :: (Ord a) => [a] -> Tree a
     list_to_bst list = foldl (\tree x -> tree_insert tree x) E list 

     bst_to_list :: (Ord a) => Tree a -> [a]
     bst_to_list E = []
     bst_to_list (B x left right)
          = (bst_to_list left) ++ [x] ++ (bst_to_list right) 

     sort :: (Ord a) => [a] -> [a]
     sort list = (bst_to_list . list_to_bst) list 
 
--     sort :: [(a,Int)] -> [(a,Int)]
--    insert :: [(Char,Int)] -> (Char,Int) -> [(Char,Int)]
--    insert []   (k,v) = [(k,v)]
--    insert list (k,v)
--        = inner list (k,v) []
--        where
--        inner [] (k,v) front 
--	    = front ++ [(k,v)]
--        inner (h:t) (k,v) front
--            | v <= snd h = front ++ [(k,v)] ++ (h:t)
--            | otherwise  = inner t (k,v) (front ++ [h]) 
--	
--    sort :: [(Char,Int)] -> [(Char,Int)]
--    sort list = foldl (\acc x -> insert acc x) [] list           
