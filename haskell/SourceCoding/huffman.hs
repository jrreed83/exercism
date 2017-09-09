{-# LANGUAGE FlexibleInstances #-}

module SourceCoding.Huffman where

     import SourceCoding.Histogram

     data Tree a = Leaf a 
                 | Branch (Tree a) (Tree a) 
                 deriving (Show)


     merge :: (Tree a,Int) -> (Tree a,Int) -> (Tree a,Int)
     merge (t1, p1) (t2, p2) 
         | p1 <= p2  = (Branch t1 t2,p1+p2)
         | otherwise = (Branch t2 t1,p1+p2) 

     insert :: [(Tree a,Int)] -> (Tree a,Int) -> [(Tree a,Int)]
     insert list node = 
         inner list node [] 
         where inner []    node accum = accum ++ [node]
               inner (h:t) node accum 
                    | (snd h) < (snd node) = inner t node (accum ++ [h])
                    | otherwise            = accum ++ [node] ++ (h:t)

     reduce :: [(Tree a,Int)] -> Tree a  
     reduce [(tree,_)] = tree
     reduce (x:y:t)    = reduce (insert t (merge x y)) 
    
     mk_table :: (Eq a) => [a] -> [(a, String)]
     mk_table list  
         = inner tree [] 
         where inner (Leaf x) (pattern) = [(x,pattern)]
               inner (Branch left right) (pattern)  
                    = inner (left) (pattern ++ "0") ++ inner (right) (pattern ++ "1")
               tree = list |> histogram |> map (\(x,c) -> (Leaf x,c)) |> reduce

     (|>) :: a -> (a -> b) -> b
     (|>) x f = f x
        
