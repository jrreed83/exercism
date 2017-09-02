module ShannonFano where

    data Tree = Nil | Branch (Char,Int) Tree Tree  

    instance Show Tree where
       show t = show (to_list t)
    
--    instance Functor Tree where
--       fmap fn Nil = Nil
--       fmap fn (Branch (k,v) left right) 
--          = Nil --Branch (fn (k,v)) (fmap left) (fmap right)

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

    data Node = Node {char :: Char 
                     ,freq :: Int
                     } deriving Show 

    (|>) :: a -> (a -> b) -> b 
    (|>) x f = f x

    -- histogram :: String -> Histogram
    -- histogram str 
    --     = foldl (\h c -> update h c) (init_histogram) str
    
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

