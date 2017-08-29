module ShannonFano where

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
                   |> foldl (\acc x -> 
                        if (snd x) < (snd acc) then x
                        else acc
                    ) (-1,100)
        where
            n  = (length list) - 1 
            fn = discrepancy list
    
    table :: [Node] -> [(Char, String)]
    table list
        = inner list "" 
            where 
                inner (h:[]) pattern = [(char h, pattern)]
                inner (h:t ) pattern
                    = inner (left) (pattern ++ "0") ++ inner (right) (pattern ++ "1")
                    where
                        (i,_) = opt_split (h:t)
                        (left, right) = split i (h:t) 

