module ShannonFano where

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
    
    histogram :: String -> Tree 
    histogram str = foldl (\t c -> update t c) Nil str

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
	
    data Node = Node {char :: Char 
                     ,freq :: Int
                     } deriving Show 

    (|>) :: a -> (a -> b) -> b 
    (|>) x f = f x

    
    split :: Int -> [a] -> ([a], [a])
    split n list 
        = (take n list, drop n list) 

    discrepancy :: [Node] -> Int -> Int 
    discrepancy list i
        = abs ( m - n )
        where
            m = list |> (take i) |> map (\t -> freq t) |> sum
            n = list |> (drop i) |> map (\t -> freq t) |> sum
    
    opt_split :: [Node] -> (Int,Int)
    opt_split list 
        = [1 .. n] |> map (\i -> (i,fn i)) 
                   |> foldl (\acc x -> if (snd x) < (snd acc) then x else acc) (-1,100)
        where
            n  = (length list) - 1 
            fn = discrepancy list
    
    table :: [Node] -> [(Char, String)]
    table list
        = inner list "" 
        where 
            inner (h:[]) pattern = [(char h, pattern)]
            inner (list) pattern
                = inner (left) (pattern ++ "0") ++ inner (right) (pattern ++ "1")
                where
                    (h:t) = list
                    (i,_) = opt_split (list)
                    (left, right) = split i (list) 

