module Huffman where

    data Tree  = Leaf String 
               | Branch Tree Tree 
               deriving (Show)
    
    data Node = Node Tree Float 
              deriving (Show)

    merge :: Node -> Node -> Node 
    merge (Node t1 p1) (Node t2 p2) 
        | p1 <= p2  = Node (Branch t1 t2) (p1+p2)
        | otherwise = Node (Branch t2 t1) (p1+p2) 

    probability :: Node -> Float
    probability (Node _ p) = p

    tree :: Node -> Tree 
    tree (Node t _) = t 

    add :: [Node] -> Node -> [Node]
    add [] node   = [node]
    add list node = 
        inner list node [] 
        where
            inner [] node accum = accum ++ [node]
            inner (h:t) node accum 
                | probability h <  (probability node) = inner t node (accum ++ [h])
                | probability h >= (probability node) = accum ++ [node] ++ (h:t)

    convert :: [(String, Float)] -> [Node]
    convert l = map (\(s,f) -> (Node (Leaf s) f)) l

    build_tree :: [Node] -> Tree  
    build_tree (h:[]) = tree h
    build_tree list   =
        build_tree new_list 
        where
            -- Pull front nodes with highest priority
            (h1:h2:t) = list
            -- 
            hh = merge h1 h2
            new_list = add t hh
    
    unwrap :: Tree -> String -> [(String, String)]
    unwrap (Leaf s) i = [(s,i)]
    unwrap (Branch l r) i = 
        unwrap l (i ++ "0") ++ unwrap r (i ++ "1")

        
        
