module Examples.Recursion
  ( countEven
  , isEven
  , keepNonNegative
  , keepNonNegativeRewrite
  , squared
  ) where

import Prelude

import Data.Array (filter, head, null, tail)
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
-- 但是裸用这几个函数在嵌套比较复杂时可读性很差，所以需要 do annotation

