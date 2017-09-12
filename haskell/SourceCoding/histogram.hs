module SourceCoding.Histogram where

     import Data.List 

     (|>) :: a -> (a -> b) -> b 
     (|>) x f = f x

     data Tree a = Leaf a 
                 | Branch (Tree a) (Tree a) 
                 deriving (Show)

     log2 :: Float -> Float
     log2 x = logBase 2.0 x 

     entropy :: [(a,Float)] -> Float
     entropy list 
          = foldl (\acc (_,p) -> acc - p * log2 p) 0.0 list	
 
     update :: (Eq a) => [(a,Int)] -> a -> [(a,Int)] 
     update list x
	 = inner list x []
         where inner []        x front = front ++ [(x,1)] 
               inner ((k,v):t) x front
                    | x == k    = front ++ [(k,v+1)] ++ t
		    | otherwise = inner t x (front ++ [(k,v)]) 
    

     histogram :: (Eq a) => [a] -> [(a,Float)] 
     histogram list 
          = list |> update_all |> normalize |> (sortBy fn)
          where 
          fn (k1,cnt1) (k2,cnt2)
               | cnt1 < cnt2  = LT
               | cnt1 == cnt2 = EQ
               | otherwise    = GT
          update_all = foldl (\hist x -> update hist x) [] 
     
     normalize :: [(a,Int)] -> [(a,Float)]    
     normalize list
          = map (\(x,f) -> (x, (realToFrac f) / total)) list
          where total = foldl (\acc (x,f) -> acc + (realToFrac f)) 0.0 list

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
                inner (Branch left right) symbol = inner left (symbol ++ ['0']) ++ inner right (symbol ++ ['1']) 
