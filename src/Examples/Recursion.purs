module Examples.Recursion
  ( (<$?>)
  , allTrue
  , cartesianProduct
  , countEven
  , factorial
  , factorialTailRec
  , factors
  , factorsV2
  , factorsV3
  , fib
  , fibTailRec
  , isEven
  , isPrime
  , keepNonNegative
  , keepNonNegativeRewrite
  , length
  , lengthTailRec
  , pairs
  , pairs'
  , primeFactors
  , reverse
  , squared
  , triples
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array (filter, head, null, tail, concatMap, cons, (..))
import Data.Foldable (foldl, product)
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
keepNonNegative = filter $ flip (>=) 0.0

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

factorsV2 :: Int -> Array (Array Int)
factorsV2 n = filter (\xs -> product xs == n) do
  i <- 1 .. n
  j <- i .. n
  [ [ i, j ] ]

factorsV3 :: Int -> Array (Array Int)
factorsV3 n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [ i, j ]

-- 这里的 guard 类似于 filter
-- 基于 do notation 如何理解？

length :: forall a. Array a -> Int
length arr =
  if null arr then
    0
  else
    1 + (length $ fromMaybe [] $ tail arr)

lengthTailRec :: forall a. Array a -> Int
lengthTailRec arr = length' arr 0
  where
  length' :: Array a -> Int -> Int
  length' arr' acc =
    if null arr' then acc
    else length' (fromMaybe [] $ tail arr') (acc + 1)

isPrime :: Int -> Boolean
isPrime 1 = false
isPrime n = (lengthTailRec $ factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  x <- 1 .. (n + 1)
  y <- x .. (n + 1)
  z <- y .. (n + 1)
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

factorial :: Int -> Int
factorial n =
  if n == 0 then
    1
  else
    n * factorial (n - 1)

factorialTailRec :: Int -> Int -> Int
factorialTailRec n acc =
  if n == 0 then acc
  else factorialTailRec (n - 1) (acc * n)

-- 折叠可分为左和右，他们的区别在于计算进行的方向，foldl 从左往右，foldr 从右往左
-- 部分情况下，两者是没用区别，比如数字累加，部分情况有区别，比如字符串合并

-- purescirpt 针对爆栈的解决方案是尾递归优化
-- 更完全的的方案是 ** trampolining ** ，可参考 purescript-free 和 purescript-tailrec 两个库
-- 把非尾递归的函数转化为尾递归的一个方法是添加一个累加结果的变量（accumulataors）

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fib :: Int -> Int
fib n =
  if n == 0 then
    0
  else if n == 1 then
    1
  else
    fib (n - 1) + fib (n - 2)

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = iter 1 1 2
  where
  iter x1 x2 count =
    if count == n then x2
    else iter x2 (x1 + x2) (count + 1)

reverse :: forall a. Array a -> Array a
reverse = foldl (flip cons) []

-- virtual file system
