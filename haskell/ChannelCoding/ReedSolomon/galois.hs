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

     type GF = Int 

     gf_log :: GF -> Int
     gf_log 1 = 0
     gf_log 2 = 5
     gf_log 3 = 1
     gf_log 4 = 3
     gf_log 5 = 2
     gf_log 6 = 6
     gf_log 7 = 4

     gf_exp :: Int -> GF 
     gf_exp 0 = 1
     gf_exp 1 = 3
     gf_exp 2 = 5
     gf_exp 3 = 4
     gf_exp 4 = 7
     gf_exp 5 = 2
     gf_exp 6 = 6 
     gf_exp 7 = 1    
     

     (|*|) :: GF -> GF -> GF
     0 |*| _ = 0
     _ |*| 0 = 0
     x |*| y = gf_exp (mod (gf_logx + gf_logy) 7)
             where gf_logx = gf_log x
                   gf_logy = gf_log y

     inverse :: GF -> GF
     inverse x = gf_exp (7 - gf_log x) -- x ^ 7 = 1 for any x

     (|/|) :: GF -> GF -> GF
     0 |/| _ = 0
     x |/| y = x |*| (inverse y)

     (|+|) :: GF -> GF -> GF
     x |+| y = ((x2.^.y2) .<<. 2) .|. ((x1.^.y1) .<<. 1) .|. (x0.^.y0)
             where x0 = x.&.1
                   y0 = y.&.1
                   x1 = (x.>>.1).&.1 
                   y1 = (y.>>.1).&.1 
                   x2 = (x.>>.2).&.1 
                   y2 = (y.>>.2).&.1 

     ntimes :: GF -> Int -> GF
     ntimes x n 
          | (mod n 2 == 0) = 0
          | (mod n 2 == 1) = x  
     
     (|**|) :: GF -> Int -> GF
     0 |**| _ = 0
     x |**| 1 = x 
     x |**| y = gf_exp ( mod (y * gf_log (x)) 7 )

     (.^.) :: Int -> Int -> Int
     x .^. y = xor x y

     (.>>.) :: Int -> Int -> Int
     x .>>. p = shiftR x p

     (.<<.) :: Int -> Int -> Int
     x .<<. p = shiftL x p 

     poly :: GF -> GF
     poly x = (3 |*| (x |**| 3)) |+| 2
     -- x^2 + 1 |+| x + 1