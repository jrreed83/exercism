module Huffman where
    data Tree a = Leaf a 
                | Branch (Tree a) (Tree a) 
                deriving (Show)
    
    data Node a = Node (Tree a) (Float)
                deriving (Show)

    merge :: Node a -> Node a -> Node a
    merge (Node t1 p1) (Node t2 p2) 
        | p1 <= p2  = Node (Branch t1 t2) (p1+p2)
        | otherwise = Node (Branch t2 t1) (p1+p2) 

    build_tree :: [(a, Float)] -> Tree a

    unwrap :: Tree a -> [(a, String)]