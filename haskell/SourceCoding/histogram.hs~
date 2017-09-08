module SourceCoding.Histogram (histogram) where

     import Data.List 

     data Tree a = E | B a (Tree a) (Tree a) 
                   deriving Show


     update :: (Eq a) => [(a,Int)] -> a -> [(a,Int)] 
     update list x
	 = inner list x []
         where inner []        x front = front ++ [(x,1)] 
               inner ((k,v):t) x front
                    | x == k    = front ++ [(k,v+1)] ++ t
		    | otherwise = inner t x (front ++ [(k,v)]) 
    

     histogram :: (Eq a) => [a] -> [(a,Int)] 
     histogram list 
          = ((sortBy fn) . (foldl (\hist x -> update hist x) [])) list
          where 
          fn (k1,cnt1) (k2,cnt2)
               | cnt1 < cnt2  = LT
               | cnt1 == cnt2 = EQ
               | otherwise    = GT
                     

     tree_insert :: (Ord a) => Tree a -> a -> Tree a
     tree_insert E                x = B x E E
     tree_insert (B y left right) x 
	| x < y      = B y (tree_insert left x) right
	| otherwise  = B y left (tree_insert right x)

     -- Convert list to binary search tree
     list_to_bst :: (Ord a) => [a] -> Tree a
     list_to_bst list = foldl (\tree x -> tree_insert tree x) E list 

     -- In order traversal
     bst_to_list :: (Ord a) => Tree a -> [a]
     bst_to_list E = []
     bst_to_list (B x left right)
          = (bst_to_list left) ++ [x] ++ (bst_to_list right) 

     -- Sort using binary search tree
     sort :: (Ord a) => [a] -> [a]
     sort list = (bst_to_list . list_to_bst) list 
         
