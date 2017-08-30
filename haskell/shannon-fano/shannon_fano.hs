module ShannonFano where

    newtype Histogram = Histogram [(Char, Int)] deriving (Show)

    init_histogram :: Histogram
    init_histogram = Histogram []

    get_value :: Histogram -> Char -> Maybe Int
    get_value (Histogram []   ) _ = Nothing 
    get_value (Histogram ((key,value):t)) c 
        | key == c = Just value 
        | otherwise = get_value (Histogram t) c 

    update :: Histogram -> Char -> Histogram
    update (Histogram l) c 
        = inner l c []
        where 
            inner []    c accum = Histogram ( (c,1) : accum)
            inner ((key,value):t) c accum 
                | key == c = Histogram ( accum ++ [(key,value+1)] ++ t)
                | otherwise = inner (t) (c) ((key,value):accum)  

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
                   |> foldl (\acc x -> if (snd x) < (snd acc) then x
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
            inner (list) pattern
                = inner (left) (pattern ++ "0") ++ inner (right) (pattern ++ "1")
                where
                    (h:t) = list
                    (i,_) = opt_split (list)
                    (left, right) = split i (list) 

