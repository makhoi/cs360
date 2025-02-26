
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fwarn-tabs #-}

-- DO NOT change the module declaration or export list!
module Art (
  Point,
  Exp(..),
  eval,
  build,

  favoriteGray,
  favoriteColor,

  gradient,
  wavy,
  weird,
  trippyR, trippyG, trippyB
) where

import Data.Data ( Data, Typeable )

import ImageUtils ( RandomDoubles, splitRandomDoubles )
import Control.Concurrent (yield)

-- You can start GHCi at the UNIX prompt with the following command:
--
--   stack ghci --ghci-options -fobject-code
--
-- You can also load the file into GHCi after starting it by typing `:load
-- Art.hs` once GHCi has started.
--
-- You can reload a file in GHCi after making changes by typing `:reload`.
--

--
-- Problem 1: Expression evaluation
--

data Exp = X           -- Value of x
         | Y           -- Value of y
         | Sin Exp     -- sin (pi * e)
         | Cos Exp     -- cos (pi * e)
         | Mul Exp Exp -- Product of e1 and e2
         | Avg Exp Exp -- Average of e1 and e2
         | Warp Exp Exp  -- New: Distortion effect
         | Blend Exp Exp Exp  -- New: 3-input blending function
         | Threshold Exp Exp Exp  -- New: Sharp thresholding effect
-- DO NOT change the deriving clause!
  deriving (Eq, Ord, Show, Data, Typeable)

type Point = (Double, Double)

eval :: Exp -> Point -> Double
eval X (x, _) = x
eval Y (_, y) = y
eval (Sin e) p = sin (pi * eval e p)
eval (Cos e) p = cos (pi * eval e p)
eval (Mul e1 e2) p = eval e1 p * eval e2 p
eval (Avg e1 e2) p = (eval e1 p + eval e2 p) / 2
eval (Warp e1 e2) p = sin (eval e1 p * eval e2 p)  -- Distortion effect
eval (Blend e1 e2 e3) p = (eval e1 p + eval e2 p + eval e3 p) / 3  -- Weighted blend
eval (Threshold e1 e2 e3) p = if eval e1 p > 0 then eval e2 p else eval e3 p  -- Sharp transitions

--
-- Problem 2: Building random expressions
--

build :: Int -> RandomDoubles -> Exp
build 0 (r:_) = if r < 0.5 then X else Y  -- Base case: return X or Y randomly
build 0 [] = X  -- Default case for empty list

build depth (r:rs)
  | depth == 1 =  -- Ensure all possible expressions appear at depth 1
      if r < 0.1 then Sin X
      else if r < 0.2 then Cos Y
      else if r < 0.3 then Mul X Y
      else if r < 0.4 then Avg X Y
      else if r < 0.5 then Warp X Y
      else if r < 0.6 then Blend X Y X
      else if r < 0.7 then Blend Y X Y
      else if r < 0.8 then Threshold X Y X
      else if r < 0.9 then Threshold Y X Y
      else Mul Y X
  | depth == 2 =  -- Ensure depth 2 encourages recursive expressions
      if r < 0.2 then Sin (build 1 rs1)
      else if r < 0.4 then Cos (build 1 rs2)
      else if r < 0.6 then Mul (build 1 rs1) (build 1 rs2)
      else if r < 0.75 then Avg (build 1 rs1) (build 1 rs2)
      else if r < 0.85 then Warp (build 1 rs1) (build 1 rs2)
      else if r < 0.95 then Blend (build 1 rs1) (build 1 rs2) (build 1 rs3)
      else Threshold (build 1 rs1) (build 1 rs2) (build 1 rs3)
  | r < 0.05  = X  
  | r < 0.1   = Y  
  | r < 0.25  = Sin (build (depth - 1) rs1)  
  | r < 0.4   = Cos (build (depth - 1) rs2)  
  | r < 0.55  = Mul (build (depth - 1) rs1) (build (depth - 1) rs2)  
  | r < 0.7   = Avg (build (depth - 1) rs1) (build (depth - 1) rs2)  
  | r < 0.8   = if depth > 2 then Warp (build (depth - 1) rs1) (build (depth - 1) rs2) else Sin X  
  | r < 0.9   = if depth > 2 then Blend (build (depth - 1) rs1) (build (depth - 1) rs2) (build (depth - 2) rs3) else Cos Y  
  | otherwise = if depth > 2 then Threshold (build (depth - 1) rs1) (build (depth - 1) rs2) (build (depth - 2) rs3) else Mul X Y  
  where 
    (rs1, rsRest) = splitRandomDoubles rs
    (rs2, rsRest2) = splitRandomDoubles rsRest
    (rs3, _) = splitRandomDoubles rsRest2
build _ [] = X  -- Default case for empty list

--
-- Problem 3: Submit Your Favorite Image
--
-- Provide (seed, depth) pairs for your favorite grayscale and color images.
--

favoriteGray :: (Int, Int)
favoriteGray = (42,7)

favoriteColor :: (Int, Int)
favoriteColor = (15,9)

--
-- Sample expressions
--

gradient :: Exp
gradient = Avg X Y

wavy :: Exp
wavy = Avg (Cos X) (Sin Y)

weird :: Exp
weird = Mul (Sin (Sin (Sin (Sin (Sin (Sin (Sin (Cos Y))))))))
            (Cos (Sin (Cos (Avg (Sin Y) (Mul X X)))))

trippyR, trippyG, trippyB :: Exp
trippyR = Sin (Avg (Mul (Mul (Mul (Cos (Mul (Sin (Cos Y)) (Avg (Avg X X) (Sin Y)))) (Avg (Sin (Mul (Sin Y) (Mul Y X))) (Cos (Cos (Mul Y Y))))) (Sin (Mul (Sin (Mul (Sin Y) (Sin Y))) (Cos (Mul (Mul Y Y) (Sin Y)))))) (Sin (Avg (Cos (Avg (Mul (Mul Y X) (Mul X X)) (Sin (Mul Y X)))) (Sin (Avg (Avg (Sin X) (Avg X X)) (Sin (Avg X Y))))))) (Cos (Cos (Avg (Sin (Sin (Avg (Mul X X) (Mul X X)))) (Sin (Sin (Sin (Sin Y))))))))

trippyG = Sin (Mul (Mul (Avg (Avg (Cos (Mul (Cos (Cos X)) (Mul (Cos X) (Avg Y X)))) (Mul (Mul (Cos (Cos Y)) (Mul (Cos X) (Mul X Y))) (Sin (Sin (Avg Y Y))))) (Cos (Mul (Avg (Sin (Sin X)) (Sin (Sin X))) (Sin (Sin (Mul X Y)))))) (Avg (Mul (Avg (Cos (Sin (Cos X))) (Avg (Mul (Sin X) (Cos Y)) (Avg (Cos X) (Cos X)))) (Avg (Avg (Sin (Cos X)) (Sin (Sin X))) (Mul (Avg (Cos X) (Avg Y X)) (Avg (Sin Y) (Sin X))))) (Mul (Cos (Cos (Mul (Avg Y Y) (Mul Y X)))) (Cos (Cos (Sin (Avg X X))))))) (Sin (Avg (Avg (Sin (Cos (Sin (Cos X)))) (Avg (Sin (Cos (Cos Y))) (Mul (Mul (Sin Y) (Mul X Y)) (Cos (Mul Y Y))))) (Cos (Avg (Mul (Mul (Cos Y) (Mul Y Y)) (Avg (Sin Y) (Cos Y))) (Mul (Mul (Mul X X) (Avg Y X)) (Cos (Sin X))))))))

trippyB = Avg (Sin (Mul (Avg (Cos (Avg (Mul (Cos (Mul X X)) (Cos (Mul X Y))) (Avg (Avg (Mul X X) (Avg Y Y)) (Avg (Cos Y) (Cos X))))) (Avg (Avg (Avg (Mul (Sin Y) (Mul X Y)) (Sin (Mul X X))) (Avg (Mul (Mul X X) (Sin Y)) (Mul (Avg X X) (Sin Y)))) (Avg (Mul (Cos (Sin Y)) (Cos (Avg X X))) (Sin (Avg (Sin Y) (Sin Y)))))) (Cos (Avg (Avg (Avg (Sin (Mul X X)) (Avg (Sin Y) (Sin X))) (Cos (Avg (Cos Y) (Avg Y X)))) (Mul (Mul (Mul (Avg X Y) (Cos X)) (Cos (Avg Y X))) (Avg (Cos (Mul Y X)) (Mul (Mul X X) (Mul Y X)))))))) (Avg (Mul (Mul (Mul (Mul (Sin (Sin (Avg X X))) (Avg (Avg (Sin Y) (Sin Y)) (Avg (Avg X X) (Cos Y)))) (Sin (Sin (Sin (Mul Y Y))))) (Avg (Cos (Avg (Avg (Avg X Y) (Mul Y X)) (Cos (Sin X)))) (Mul (Sin (Sin (Sin X))) (Cos (Mul (Mul Y Y) (Cos X)))))) (Avg (Cos (Cos (Sin (Cos (Avg X Y))))) (Mul (Sin (Mul (Cos (Avg Y X)) (Sin (Cos X)))) (Mul (Mul (Sin (Cos Y)) (Avg (Avg X X) (Cos X))) (Avg (Mul (Sin X) (Avg Y X)) (Sin (Sin X))))))) (Mul (Mul (Cos (Cos (Mul (Sin (Mul Y Y)) (Cos (Cos X))))) (Avg (Sin (Avg (Cos (Sin Y)) (Mul (Cos X) (Avg X X)))) (Cos (Cos (Cos (Avg X Y)))))) (Sin (Mul (Avg (Mul (Cos (Mul Y Y)) (Cos (Sin Y))) (Avg (Mul (Mul X X) (Sin X)) (Cos (Sin Y)))) (Avg (Sin (Mul (Avg Y X) (Avg X X))) (Cos (Avg (Mul Y Y) (Avg Y Y))))))))