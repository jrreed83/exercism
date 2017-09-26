module ReedSolomon.Galois where

     class Field a where
          plus    :: a -> a -> a
          times   :: a -> a -> a
          one     :: a
          zero    :: a
          inverse :: a -> Maybe a
          negate  :: a -> a

     data Z2 = Z2 Int 

     instance Show Z2 where
          show (Z2 x) = show (mod x 2)

     instance Eq Z2 where
          (==) (Z2 x) (Z2 y) = (mod (x-y) 2 == 0)
          (/=) (Z2 x) (Z2 y) = (mod (x-y) 2 /= 0)

     instance Field Z2 where
          plus  (Z2 x) (Z2 y) = Z2 (mod (x+y) 2)
          times (Z2 x) (Z2 y) = Z2 (mod (x*y) 2)
          
