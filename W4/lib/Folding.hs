module Folding where

import Data.Ord
import Data.List.Ordered

fun1 :: [Int] -> Int
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Int] -> Int
fun1' = product . map ((-2) +) . filter even

fun2 :: Int -> Int
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Int -> Int
fun2' n = sum (filter even (takeWhile (/= 1) (iterate (\x -> if even x then x `div` 2 else 3*x + 1) n)))

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

data BinaryTree a = LeafTwo
                  | NodeTwo (BinaryTree a) a (BinaryTree a)


xor :: [Bool] -> Bool
-- xor l = odd (foldl (\acc x -> if x then acc + 1 else acc + 0) 0 l)
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x next -> f x : next) []

sieveSundaram :: Int -> [Int]
-- sieveSundaram n = map (\x -> 2*x + 1) (filter (\x -> x `notElem` [i + j + 2*i*j | i <- [1..n], j <- [i..n], i + j + 2*i*j <= n] ) [1..n])
sieveSundaram n = map (\x -> 2*x + 1) (filter (\x -> x `notElem` [i+j+2*i*j|let n'=fromIntegral n,
                           i<-[1..floor (sqrt (n' / 2))],
                           let i' = fromIntegral i,
                           j<-[i..floor( (n'-i')/(2*i'+1))]] ) [1..n]) 

sieveSundaram' :: Int -> [Int]
sieveSundaram' n = map (\x -> 2*x + 1) ( minus [1..n] (sort [i+j+2*i*j|let n'=fromIntegral n,
                           i<-[1..floor (sqrt (n' / 2))],
                           let i' = fromIntegral i,
                           j<-[i..floor( (n'-i')/(2*i'+1))]]) )
