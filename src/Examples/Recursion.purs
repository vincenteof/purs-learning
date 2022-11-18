module Examples.Recursion
  ( cartesianProduct
  , countEven
  , factors
  , factors'
  , isEven
  , isPrime
  , keepNonNegative
  , keepNonNegativeRewrite
  , pairs
  , pairs'
  , primeFactors
  , squared
  , triples
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array (filter, head, null, tail, concatMap, length, cons, (..))
import Data.Foldable (product)
import Data.Maybe (fromMaybe)

isEven :: Int -> Boolean
isEven n
  | n < 0 = isEven (-n)
  | n == 0 = true
  | otherwise = not (isEven (n - 1))

countEven :: Array Int -> Int
countEven arr
  | null arr = 0
  | otherwise = headResult + tailResult
      where
      oneIfEven num = if isEven num then 1 else 0
      headResult = fromMaybe 0 $ oneIfEven <$> head arr
      tailResult = countEven $ fromMaybe [] $ tail arr

-- 直接 map Maybe functor

-- .. 是个 infix operator 1..5 这个语法直接为 range 1 5

squared :: Array Number -> Array Number
squared = map (\n -> n * n)

keepNonNegative :: Array Number -> Array Number
-- 因为类型是 Number，需要用 0.0
keepNonNegative = filter $ (>) 0.0

infixr 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
-- 因为是 infix，所以参数倒过来传
keepNonNegativeRewrite arr = (\n -> n >= 0.0) <$?> arr

-- purescript 没有 list comprehension，基本上就是靠 map, filter, concatMap
-- 但是裸用这几个函数在嵌套比较复杂时可读性很差，所以需要 do notation
pairs :: Int -> Array (Array Int)
pairs n = concatMap
  ( \i ->
      map (\j -> [ i, j ]) (i .. n)
  )
  (1 .. n)

-- list do notation 到底啥意思
pairs' :: Int -> Array (Array Int)
pairs' n = do
  i <- 1 .. n
  j <- i .. n
  pure [ i, j ]

-- monad 的 bind 类型为 m a -> (a -> m b) -> m b
-- 实现 list monad 只需要  xs >>= f = concatMap f xs

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) (pairs' n)

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [ i, j ]

-- 这里的 guard 类似于 filter

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  x <- 1 .. (n + 1)
  y <- 1 .. (n + 1)
  z <- 1 .. (n + 1)
  guard $ x * x + y * y == z * z
  pure [ x, y, z ]

primeFactors :: Int -> Array Int
primeFactors n = calc 2 n
  where
  calc _ 1 = []
  calc x y =
    if y `mod` x == 0 then cons x $ calc x (y / x)
    else calc (x + 1) y
-- 因为非质数总是质数的 product，所以非质数不可能满足整除，否则计算就会卡在把它整除的质数上