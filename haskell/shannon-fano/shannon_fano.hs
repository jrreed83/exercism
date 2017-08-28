module ShannonFano where

    data Node = Node {char :: Char 
                     ,freq :: Int
                     } deriving Show 

    instance Eq Node where 
        (==) node1 node2
            = (freq node1) == (freq node2)
        
        (/=) node1 node2
            = (freq node1) /= (freq node2) 

    split :: Int -> [a] -> ([a], [a])
    split n list 
        = (take n list, drop n list) 

    total :: [Node] -> Int
    total list 
        = foldl (\acc node -> acc + (freq node)) 0 list 

    node_list_split :: Int -> [Node] -> ([Node], [Node], Int)
    node_list_split i list = 
        let (left, right) = split i list
            diff = discrepancy left right
        in (left, right, diff)

    discrepancy :: [Node] -> [Node]  -> Int
    discrepancy list1 list2
        = abs ((total list1) - (total list2))

    all_splits :: [Node] -> [([Node], [Node], Int)]
    all_splits list
        = map (\i -> node_list_split i list) [1..length(list)-1]
    
