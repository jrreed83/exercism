module SourceCoding.Histogram where

     import Data.List 

     (|>) :: a -> (a -> b) -> b 
     (|>) x f = f x

     data Tree a = Leaf a 
                 | Branch (Tree a) (Tree a) 
                 deriving (Show)

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
         
     encode :: (Eq a) => Tree a -> [(a,String)]
     encode tree
          = inner tree []
          where inner (Leaf x)            symbol = [(x,symbol)]
                inner (Branch left right) symbol = inner left (symbol ++ "0") ++ inner right (symbol ++ "1") 
