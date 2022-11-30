module Examples.PatternMatching
  ( Address
  , Amp(..)
  , Ohm(..)
  , Person
  , Volt(..)
  , Watt(..)
  , binomial
  , calculateCurrent
  , calculateWattage
  , current
  , factorial
  , fromSingleton
  , fromString
  , gcd
  , gcdV2
  , isEmpty
  , livesInLA
  , lzs
  , partialFunction
  , pascal
  , sameCity
  , showPerson
  , showPersonV2
  , sortPair
  , takeFive
  , toString
  , unknownPerson
  ) where

import Prelude hiding (gcd)
import Partial.Unsafe (unsafePartial)

import Data.Array (tail)
import Data.Foldable (sum)
import Data.Maybe (fromMaybe)

partialFunction :: Boolean -> Boolean
partialFunction = unsafePartial \true -> true

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

-- newtype 可以构造一种特殊的 ADT，只有一个 constructor 并且只接受一个参数
newtype Volt = Volt Number
newtype Ohm = Ohm Number
newtype Amp = Amp Number

derive newtype instance eqAmp :: Eq Amp
derive newtype instance showAmp :: Show Amp

calculateCurrent :: Volt -> Ohm -> Amp
calculateCurrent (Volt v) (Ohm r) = Amp (v / r)

battery :: Volt
battery = Volt 1.5

lightbulb :: Ohm
lightbulb = Ohm 500.0

current :: Amp
current = calculateCurrent battery lightbulb

-- 这个类型的意义在于，比如上面的公式的参数其实是不同单位的，如果是同一个单位的参数会发生 compilation error
-- 如果设计成 number 类型的话就丧失了这种语意

newtype Coulomb = MakeCoulomb Number
-- 一般来说 type constructor 和 data constructor 都是同名的，但是不一样也是允许的，不过一般不这样

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)
