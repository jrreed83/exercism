{-# LANGUAGE FlexibleInstances #-}

module SourceCoding.Huffman where

     import SourceCoding.Histogram 

     merge :: (Tree a,Int) -> (Tree a,Int) -> (Tree a,Int)
     merge (t1, p1) (t2, p2) 
         -- Greater probability/frequency => Lower priority
         | p1 >= p2  = (Branch t1 t2,p1+p2) 
         | otherwise = (Branch t2 t1,p1+p2) 

     insert :: [(Tree a,Int)] -> (Tree a,Int) -> [(Tree a,Int)]
     insert list node = 
         inner list node [] 
         where inner []    node accum = accum ++ [node]
               inner (h:t) node accum 
                    | (snd h) < (snd node) = inner t node (accum ++ [h])
                    | otherwise            = accum ++ [node] ++ (h:t)

     huffman_tree :: [(a,Int)] -> Tree a  
     huffman_tree list 
          = inner (map (\(x,i) -> (Leaf x, i)) list)
          where inner [(tree,_)] = tree
                inner (x:y:t)    = inner (insert t (merge x y)) 
    

