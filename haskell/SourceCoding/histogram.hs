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
         
     tbl_lookup :: (Eq a) => [(a,String)] -> a -> Maybe String
     tbl_lookup [] x = Nothing
     tbl_lookup (h:t) x
          | (fst h) == x = Just (snd h)
          | otherwise    = tbl_lookup t x     

     encode :: (Eq a) => [(a,String)] -> [a] -> Maybe String 
     encode tbl list
          = inner tbl list []
          where inner tbl []    accum = Just accum
                inner tbl (h:t) accum
                     = case tbl_lookup tbl h of
                            Nothing -> Nothing
                            Just s  -> inner tbl t (accum ++ s)

     tree_unwrap :: (Eq a) => Tree a -> [(a,String)]
     tree_unwrap tree
          = inner tree []
          where inner (Leaf x)            symbol = [(x,symbol)]
                inner (Branch left right) symbol = inner left ('0':symbol) ++ inner right ('1':symbol) 
