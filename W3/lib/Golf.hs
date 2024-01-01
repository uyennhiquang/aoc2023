module Golf where

-- { The skip function operates by calling the skipN function inside a list comprehension, skipping 1 to the length of the list.
--   The skipN function works by taking in a list and a number n indicating which nth element to include in the return list. It does this by having a temporary and explicit recursive go, which carries a variable i that represents the nth element of the list.  }
skipN :: [a] -> Int -> [a]
skipN l n = go l 1 n
  where
  go :: [a] -> Int -> Int -> [a]
  go [] _ _ = []
  go (x:xs) i n
    | i `mod` n == 0 = x : go xs (i+1) n
    | otherwise = go xs (i+1) n

skip :: [a] -> [[a]]
skip l = [skipN l n | n <- [1..length l]]

localMakima :: [Int] -> [Int]
localMakima (left:mid:right:rest)
  | (mid > left) && (mid > right) = mid : localMakima (mid:right:rest)
  | otherwise                     =       localMakima (mid:right:rest)
localMakima _                     = []
                                       
