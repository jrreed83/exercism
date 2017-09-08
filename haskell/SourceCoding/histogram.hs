module SourceCoding.Histogram (histogram) where

     import Data.List 


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
         
