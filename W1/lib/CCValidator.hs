module CCValidator where

toDigits :: Int -> [Int]
toDigits n = go n []
  where
  go :: Int -> [Int] -> [Int]
  go i l
    | i <= 0 = l
    | otherwise = go (i `div` 10) ((i `mod` 10):l)

toDigitsRev :: Int -> [Int]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther l = go l [] 0 (length l)
  where
  go :: [Int] -> [Int] -> Int -> Int -> [Int]
  go il ol i len = if i == len
                  then ol
                  else go (tail il) (ol ++ [num]) (i + 1) len
                    where
                    num :: Int
                    num = if even i
                             then head il * 2
                             else head il

sumDigits :: [Int] -> Int
sumDigits l = sum (map (sum . toDigits) l)

validate :: Int -> Bool
validate n 
  | mod ((sumDigits . doubleEveryOther . toDigits) n) 10 == 0 = True
  | otherwise = False 
