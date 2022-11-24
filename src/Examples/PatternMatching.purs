module Examples.PatternMatching
  ( Address
  , Person
  , binomial
  , factorial
  , fromSingleton
  , fromString
  , gcd
  , isEmpty
  , livesInLA
  , lzs
  , pascal
  , sameCity
  , showPerson
  , showPersonV2
  , sortPair
  , takeFive
  , toString
  , unknownPerson
  ) where

import Prelude

import Data.Array (tail)
import Data.Foldable (sum)
import Data.Maybe (fromMaybe)

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m =
  if n > m then gcd (n - m) m
  else gcd n (m - n)

fromString :: String -> Boolean
fromString "true" = true
fromString _ = false

toString :: Boolean -> String
toString true = "true"
toString false = "false"

gcdV2 :: Int -> Int -> Int
gcdV2 n 0 = n
gcdV2 0 m = m
gcdV2 n m
  | n > m = gcdV2 (n - m) m
  | otherwise = gcdV2 n (m - n)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- n 个里取 k 个
binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k
  | n < k = 0
  | otherwise = factorial n / (factorial k * (factorial (n - k)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
takeFive [ 0, 1, a, b, _ ] = a * b
takeFive _ = 0

-- but PureScript does not provide any means of matching arrays of an unspecified length, since destructuring immutable arrays in these sorts of ways can lead to poor performance. 
-- 这是什么意思，具体指什么操作？
-- Data.List 与 Data.Array 内部的数据结构到底是什么样？

showPerson :: { first :: String, last :: String } -> String
showPerson { first: x, last: y } = y <> ", " <> x

-- 如果不手动把定义写出来的话，会被自动推导为
-- forall r. { first :: String, last :: String | r } -> String
-- 表示出了 first 和 last，还有其他任意 field
-- 这称为 ** row polymorphism **

showPersonV2 :: { first :: String, last :: String } -> String
showPersonV2 { first, last } = last <> ", " <> first

-- 类似于直接解构

unknownPerson :: { first :: String, last :: String }
unknownPerson = { first, last }
  where
  first = "Jane"
  last = "Doe"

-- 这也是类似于 JS 的语法

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: city1 } } { address: { city: city2 } } = city1 == city2

-- 最 general 的类型为
-- 最多可以添加 4 个 row，更泛化是 city 只要支持 Eq 也可以

-- 嵌套

sortPair :: Array Int -> Array Int
sortPair arr@[ x, y ]
  | x <= y = arr
  | otherwise = [ y, x ]
sortPair arr = arr

-- 对 pattern 取名

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x
fromSingleton default _ = default

-- 最长的和为 0 的后缀
lzs :: Array Int -> Array Int
lzs [] = []
lzs xs = case sum xs of
  0 -> xs
  _ -> lzs (fromMaybe [] $ tail xs)
