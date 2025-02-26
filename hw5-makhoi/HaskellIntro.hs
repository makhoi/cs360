{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

-- You can start GHCi at the UNIX prompt with the command `stack ghci`.
--
-- You can also load the file into GHCi after starting it by typing `:load
-- HaskellIntro.hs` once GHCi has started.
--
-- You can reload a file in GHCi after making changes by typing `:reload`.
--
-- Load this file into GHCi and type `isThisWorking` at the prompt. GHCi will
-- tell you whether it's working!
--

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit n = abs n `mod` 10 

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10 

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . helper . reverse
  where
    helper [] = []
    helper [x] = [x]
    helper (x:y:zs) = x : (2 * y) : helper zs


sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concatMap toIndividualDigits xs)

toIndividualDigits :: Integer -> [Integer]
toIndividualDigits n
  | n < 10    = [n]  
  | otherwise = toIndividualDigits (n `div` 10) ++ [n `mod` 10] 


validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0


--
-- Problem 2
--

data Wobbly a = Stable a | Wobbly a
  deriving (Eq, Ord, Show)

onlyStable :: [Wobbly a] -> [a]
onlyStable ws = [ a | Stable a <- ws ]

mapWobbly :: (a -> b) -> [Wobbly a] -> [Wobbly b]
mapWobbly _ [] = []
mapWobbly f (x:xs) = case x of
  Stable a  -> Stable (f a) : mapWobbly f xs
  Wobbly a  -> Wobbly (f a) : mapWobbly f xs



splitWobbly :: [Wobbly a] -> ([a], [a])
splitWobbly ws = ( [ a | Stable a <- ws ]
                 , [ a | Wobbly a <- ws ]
                 )

--
-- Problem 3
--
pow :: (a -> a) -> Int -> a -> a
pow _ 0 = id
pow f n = f . pow f (n - 1)

g :: Integer -> Integer
g 0 = 0
g n = n - pow g 2 (n - 1)

h :: Integer -> Integer
h 0 = 0
h 1 = 1
h n = n - pow h 3 (n - 1)

d :: Int -> Integer -> Integer
d _ 0 = 0
d i n = n - pow (d i) i (n - 1)
