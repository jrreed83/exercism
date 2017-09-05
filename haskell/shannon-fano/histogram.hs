module SourceCoding.Histogram (histogram) where

    data Tree = Nil | Branch (Char,Int) Tree Tree  

    instance Show Tree where
       show t = show (to_list t)
    
    init_tree :: Tree 
    init_tree = Nil 

    to_list :: Tree -> [(Char, Int)] 
    to_list Nil = [] 
    to_list (Branch (key, cnt) left right)
       = to_list(left) ++ [(key, cnt)] ++ to_list(right) 

    find :: Tree -> Char -> Maybe Int 
    find Nil _  = Nothing
    find (Branch (key, cnt) left right) char 
        | char == key = Just cnt 
        | char <  key = find left char
        | char >  key = find right char

    update :: Tree -> Char -> Tree 
    update Nil char  
        = Branch (char, 1) Nil Nil 
    update (Branch (key, cnt) left right) char 
        | char == key = (Branch (key, cnt+1) left right)
        | char <  key = Branch (key,cnt) (update left char) right 
        | char >  key = Branch (key,cnt) left (update right char) 
    
    build_tree :: String -> Tree
    build_tree str = foldl (\t c -> update t c) Nil str

    histogram :: String -> [(Char,Int)] 
    histogram str = (sort . to_list . build_tree) str 

    insert :: [(Char,Int)] -> (Char,Int) -> [(Char,Int)]
    insert []   (k,v) = [(k,v)]
    insert list (k,v)
        = inner list (k,v) []
        where
        inner [] (k,v) front 
	    = front ++ [(k,v)]
        inner (h:t) (k,v) front
            | v <= snd h = front ++ [(k,v)] ++ (h:t)
            | otherwise  = inner t (k,v) (front ++ [h]) 
	
    sort :: [(Char,Int)] -> [(Char,Int)]
    sort list = foldl (\acc x -> insert acc x) [] list           
