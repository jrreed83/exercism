{-# LANGUAGE FlexibleInstances #-}

module SourceCoding.Huffman where

     import SourceCoding.Histogram 

     huffman_encode :: (Eq a) => [a] -> String
     huffman_encode list
          = encode tbl list
          where tbl = list |> histogram |> huffman_tree |> tree_unwrap  

     merge :: (Fractional b,Ord b) => (Tree a,b) -> (Tree a,b) -> (Tree a,b)
     merge (t1, p1) (t2, p2) 
         -- Greater probability/frequency => Lower priority
         | p1 >= p2  = (Branch t2 t1,p1+p2) 
         | otherwise = (Branch t1 t2,p1+p2) 

     insert :: (Fractional b,Ord b) => [(Tree a,b)] -> (Tree a,b) -> [(Tree a,b)]
     insert list node = 
         inner list node [] 
         where inner []    node accum = accum ++ [node]
               inner (h:t) node accum 
                    | (snd h) < (snd node) = inner t node (accum ++ [h])
                    | otherwise            = accum ++ [node] ++ (h:t)

     huffman_tree :: (Fractional b, Ord b) => [(a,b)] -> Tree a  
     huffman_tree list 
          = inner (map (\(x,i) -> (Leaf x, i)) list)
          where inner [(tree,_)] = tree
                inner (x:y:t)    = inner (insert t (merge x y)) 
    

