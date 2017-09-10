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

     new_split :: (Eq a) => [(a,Int)] -> ([(a,Int)],[(a,Int)])
     new_split list
          = inner list (sum (map (\t -> snd t) list)) [] 0
          where inner right@(h:t) rsum left lsum
                     | abs(rsum-lsum) <= abs(new_rsum-new_lsum) = (left,right)
                     | otherwise    = inner t new_rsum (left ++ [h]) new_lsum
                     where new_rsum = rsum - (snd h)
                           new lsum = lsum + (snd h)
