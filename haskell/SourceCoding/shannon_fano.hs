module SourceCoding.ShannonFano where
	
     import SourceCoding.Histogram
     import Data.List

     shannon_fano_encode :: (Eq a) => [a] -> Maybe String
     shannon_fano_encode list
          = encode tbl list
          where tbl = list |> histogram |> shannon_fano_tree |> tree_unwrap    

     shannon_fano_tree :: (Fractional b, Ord b) => [(a,b)] -> Tree a  
     shannon_fano_tree [(x,_)]    = Leaf x
     shannon_fano_tree list@(h:t) = Branch (shannon_fano_tree left) (shannon_fano_tree right)
          where (left, right) = split (list) 

     split :: (Fractional b, Ord b) => [(a,b)] -> ([(a,b)],[(a,b)])
     split list
          = inner list [] delta
          where inner right@(h:t) left delta
                     | abs(delta) <= abs(new_delta) = (left,right)
                     | otherwise   = inner t (left ++ [h]) new_delta
                     where new_delta = delta - 2 * (snd h)
                delta = (sum (map (\t -> snd t) list)) 
