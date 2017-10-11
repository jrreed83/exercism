{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleInstances #-}

module ReedSolomon.Galois where

     -- The field we will first work with is GF(8) ~ Z2/(x^3 + x + 1) = {0,1,x,x^2,x+1,x^2+1,x^2+x,x^2+x+1}
     -- In binary, these elements can be represented as {000,001,010,100,011,101,110,111}, which in turn can be
     -- represented as {0,1,2,4,3,5,6,7}.  This is a field because the polynomial is reducible.  Moreover, the 
     -- non-zero elements form a multiplicative cyclic group.  If we can find a generator, then we can perform multiplication
     -- via discrete logarithms.  The multiplicative subgroup has order 7 and therefore by Lagrange's theorem, every
     -- non-trivial element generates the entire group
     --
     -- (x+1)^1 = x+1 
     -- (x+1)^2 = x^2 + 2x + 1 = x^2 + 1 
     -- (x+1)^3 = (x^2 + 1)(x+1) = x^3 + x^2 + x + 1 = (x+1) + x^2 + x + 1 = x^2
     -- (x+1)^4 = (x^2 + 1)(x^2 + 1) = x^4 + 1 = x(x+1) + 1 = x^2 + x + 1
     -- (x+1)^5 = (x+1)(x^2 + x + 1) = x^3 + x^2 + x + x^2 + x + 1 = x^3 + 1 = x+1 + 1 = x
     -- (x+1)^6 = (x+1)(x) = x^2 + x
     -- (x+1)^7 = (x+1)(x^2 + x) = x(x+1)^2 = x(x^2 + 1) = x^3 + x = x+1+x = 1
 
     -- Multiplication in the field can be performed with discrete logarithms:
     -- x * y = z^m * z^n = z^p, where p = (m+n) mod 7
     
     -- Here's the logarithm table
     -- 3 -> 1
     -- 5 -> 2
     -- 4 -> 3
     -- 7 -> 4
     -- 2 -> 5
     -- 6 -> 6
     -- 1 -> 7 (or 0)
     --
     -- Here's the exponent table (mod 7)
     -- 1 -> 3
     -- 2 -> 5
     -- 3 -> 4
     -- 4 -> 7
     -- 5 -> 2
     -- 6 -> 6
     -- 0 -> 1
     --
     --

     import qualified Data.Map as Map 
     import Data.Bits
     import Data.Word      

     type GF8 = Int 

     data Bit = L | H

     instance Show Bit where
          show L = "0"
          show H = "1"

     gf_log :: GF8 -> Int
     gf_log 1 = 0
     gf_log 2 = 5
     gf_log 3 = 1
     gf_log 4 = 3
     gf_log 5 = 2
     gf_log 6 = 6
     gf_log 7 = 4
     gf_log x | x > 7 = gf_log (mod x 7)

     gf_exp :: Int -> GF8 
     gf_exp 0 = 1
     gf_exp 1 = 3
     gf_exp 2 = 5
     gf_exp 3 = 4
     gf_exp 4 = 7
     gf_exp 5 = 2
     gf_exp 6 = 6 
     gf_exp 7 = 1  
     gf_exp x | x > 7 = gf_exp (mod x 7)  
     
     data GF = GF Int

     instance Show GF where
          show (GF x) = "GF " ++ (show (mod x 7))

     instance Num GF where
          (GF x) + (GF y) = GF ( xor x y )
          (GF x) * (GF y) = GF ( gf_exp (gf_log x + gf_log y))
          abs (GF x) = GF x
                         
     instance Fractional GF where
          (GF x) / (GF y) = (GF x) * (GF (gf_exp (7 - gf_log y)))

     instance Eq GF where
          (GF x) == (GF y) = (mod (abs(x-y)) 7) == 0
          (GF x) /= (GF y) = (mod (abs(x-y)) 7) /= 0
 

     (.^.) :: Integer -> Integer -> Integer
     x .^. y = xor x y

     (.>>.) :: Integer -> Int -> Integer
     x .>>. p = shiftR x p

     (.<<.) :: Integer -> Int -> Integer
     x .<<. p = shiftL x p 

     poly :: GF -> GF
     poly x = (a3 * x^3) + (a1 * x) + a0 
            where a3 = GF 3
                  a1 = GF 2
                  a0 = GF 1 
     -- x^2 + 1 |+| x + 1
     --
     degree :: Integer -> Int
     degree 0 = 0
     degree 1 = 0
     degree x =
          degree' x 0
          where degree' 0 n = (n-1)
                degree' y i = degree' (y .>>. 1) (i+1)

     order :: Integer -> Int
     order x = 
          order' x 1
          where order' 1 n = n
                order' y n = order' ((y |*| x) % 13) (n+1)
    
     (|*|) :: Integer -> Integer -> Integer
     x |*| y = 
          inner y (toInteger 0) 0
          where inner yi result n 
                     | yi       == 0 = result
                     | yi .&. 1 == 0 = inner (yi .>>. 1) result (n+1)
                     | yi .&. 1 == 1 = inner (yi .>>. 1) (xor result (x .<<. n)) (n+1)

     (%) :: Integer -> Integer -> Integer
     x % y
          | degree x < degree y = x
          | otherwise           = (xor x (y .<<. ((degree x) - (degree y)))) % y

     crcEncode :: Integer -> Integer -> Integer
     crcEncode genPoly msg = (msg .<<. (degree genPoly)) % genPoly

     crcDecode :: Integer -> Integer -> Integer
     crcDecode genPoly msg = msg % genPoly

     data Conversion = Bits | Polynomial

     toBits :: Integer -> String
     toBits 0 = []
     toBits x = toBits (x .>>. 1) ++ show (x .&. 1) 

-- x + 1 
