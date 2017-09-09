module SourceCoding.ShannonFano where
	
    import SourceCoding.Histogram
    import Data.List


    shannon_fano_tree :: (Eq a) => [(a,Int)] -> Tree a
    shannon_fano_tree [(x,_)]    = Leaf x
    shannon_fano_tree list@(h:t) = Branch (shannon_fano_tree left) (shannon_fano_tree right)
         where i             = opt_split (list)
               (left, right) = split i (list) 

    split :: Int -> [a] -> ([a], [a])
    split n list 
        = (take n list, drop n list) 

    discrepancy :: [(a,Int)] -> Int -> Int 
    discrepancy list i
        = abs ( m - n )
        where m = list |> (take i) |> map (\t -> snd t) |> sum
              n = list |> (drop i) |> map (\t -> snd t) |> sum
    
    opt_split :: (Eq a) => [(a,Int)] -> Int
    opt_split list 
        = [1 .. n] |> map (\i -> (i,(discrepancy list i))) 
                   |> foldl (\acc x -> if (snd x) < (snd acc) then x else acc) (-1,100)
		   |> fst
        where n  = (length list) - 1 

    
    --find :: (Eq a) => [(a,b)] -> a -> Maybe b
    --find []    c = Nothing
    --find (h:t) c 
    --     | fst h == c = Just (snd h)
    --     | otherwise  = find t c
	
              
    --encode :: (Eq a) => [(a,String)] -> [a] -> Maybe String
    --encode tbl []   = Nothing
    --encode tbl list
    --   = inner list (Just [])
    --   where  inner []    accum = accum
    --          inner (h:t) accum 
    --               = case (find tbl h) of 
    --                      Nothing -> Nothing
    --                      Just x -> inner t (do { y <- accum; return (y ++ x)})
        
