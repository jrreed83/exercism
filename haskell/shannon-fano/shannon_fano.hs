module SourceCoding.ShannonFano where
	
    import SourceCoding.Histogram


    (|>) :: a -> (a -> b) -> b 
    (|>) x f = f x

    
    split :: Int -> [a] -> ([a], [a])
    split n list 
        = (take n list, drop n list) 

    discrepancy :: [(Char,Int)] -> Int -> Int 
    discrepancy list i
        = abs ( m - n )
        where
        m = list |> (take i) |> map (\t -> snd t) |> sum
        n = list |> (drop i) |> map (\t -> snd t) |> sum
    
    opt_split :: [(Char,Int)] -> Int
    opt_split list 
        = [1 .. n] |> map (\i -> (i,fn i)) 
                   |> foldl (\acc x -> if (snd x) < (snd acc) then x else acc) (-1,100)
		   |> fst
        where
        n  = (length list) - 1 
        fn = discrepancy list
    
    mk_table :: [(Char,Int)] -> [(Char, String)]
    mk_table list
        = inner list "" 
        where 
        inner (h:[]) pattern = [(fst h, pattern)]
        inner (list) pattern
            = inner (left) (pattern ++ "0") ++ inner (right) (pattern ++ "1")
            where
            (h:t)         = list
            i             = opt_split (list)
            (left, right) = split i (list) 
    
    lookup_ :: [(Char,String)] -> Char -> Maybe String
    lookup_ tbl c
        = case filter (\(k,v) -> k == c) tbl of
              []  -> Nothing
              h:t -> Just (snd h)
              
    encode :: [(Char,String)] -> String -> Maybe String
    encode tbl []  = Nothing
    encode tbl str
       = inner str (Just [])
       where
       inner []    accum = accum
       inner (h:t) accum 
           = case (lookup_ tbl h) of 
                 Nothing -> Nothing
                 Just x -> inner t (do { y <- accum; return (y ++ x)})
        
