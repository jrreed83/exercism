module Huffman where

    data Tree  = Leaf String 
               | Branch Tree Tree 
               deriving (Show)
    
    data Node = Node { tree :: Tree 
                     , probability :: Float
                     } deriving (Show)

    merge :: Node -> Node -> Node 
    merge (Node t1 p1) (Node t2 p2) 
        | p1 <= p2  = Node (Branch t1 t2) (p1+p2)
        | otherwise = Node (Branch t2 t1) (p1+p2) 

    add :: [Node] -> Node -> [Node]
    add list node = 
        inner list node [] 
        where
            inner []    node accum = accum ++ [node]
            inner (h:t) node accum 
                | probability h <  (probability node) = inner t node (accum ++ [h])
                | probability h >= (probability node) = accum ++ [node] ++ (h:t)

    convert :: [(String, Float)] -> [Node]
    convert l = map (\(s,f) -> (Node (Leaf s) f)) l

    build_tree :: [Node] -> Tree  
    build_tree (h:[]) = tree h
    build_tree (list) =
        build_tree (new_list) 
        where
            (h1:h2:t) = list
            hh = merge (h1) (h2)
            new_list = add (t) (hh)
    
    unwrap :: Tree -> [(String, String)]
    unwrap t = 
        inner t "" 
        where
            inner (Leaf symbol) (pattern) = 
                [(symbol,pattern)]
            inner (Branch left right) (pattern) = 
                inner (left) (pattern ++ "0") ++ inner (right) (pattern ++ "1")

    huffman :: [(String, Float)] -> [(String, String)]
    huffman list = list |> convert |> build_tree |> unwrap 

    (|>) :: a -> (a -> b) -> b
    (|>) x f = f x
        
