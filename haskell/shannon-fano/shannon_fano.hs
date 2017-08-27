module ShannonFano where

    split :: Int -> [a] -> ([a], [a])
    split n list 
        = (take n list, drop n list) 

    total :: [(Char,Int)] -> Int
    total list 
        = foldl (\acc node -> acc + (snd node)) 0 list 

    node_list_split :: Int -> [(Char,Int)] -> ([(Char,Int)], [(Char,Int)], Int)
    node_list_split i list = 
        let (left, right) = split i list
            diff = discrepancy left right
        in (left, right, diff)

    discrepancy :: [(Char,Int)] -> [(Char,Int)]  -> Int
    discrepancy list1 list2
        = abs ((total list1) - (total list2))

    all_splits :: [(Char,Int)] -> [([(Char,Int)], [(Char,Int)], Int)]
    all_splits list
        = map (\i -> node_list_split i list) [1..length(list)-1]
    