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

     gf_log :: Int -> Int
     gf_log 1 = 0
     gf_log 2 = 5
     gf_log 3 = 1
     gf_log 4 = 3
     gf_log 5 = 2
     gf_log 6 = 6
     gf_log 7 = 4

     gf_exp :: Int -> Int 
     gf_exp 0 = 1
     gf_exp 1 = 3
     gf_exp 2 = 5
     gf_exp 3 = 4
     gf_exp 4 = 3
     gf_exp 5 = 2
     gf_exp 6 = 6 
     gf_exp 7 = 1    

     (|*|) :: Int -> Int -> Int
     x |*| y 
          = gf_exp p
            where gf_logx = gf_log x
                  gf_logy = gf_log y
                  p    = mod (gf_logx + gf_logy) 7

     inverse :: Int -> Int
     inverse x    
          = gf_exp m
            where m = (7 - gf_log x)  

     (|/|) :: Int -> Int -> Int
     x |/| y = x |*| (inverse y)
