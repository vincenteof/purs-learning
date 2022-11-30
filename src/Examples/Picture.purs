module Examples.Picture
  ( Picture
  , Point
  , Shape(..)
  , area
  , bounds
  , centerShape
  , circleAtOrigin
  , doubleScaleAndCenter
  , getCenter
  , origin
  , scaleShape
  , shapeBounds
  , shapeText
  , showPicture
  , showPoint
  , showShape
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Number (pi, infinity)

-- 没有匹配到的 pattern matching 会导致 runtime error
-- 尽量写 total function 避免这种情况，应该用 Maybe 去明确地处理
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture Point Number Number

-- 貌似这个 type 直接是 semiring，所以可以直接用 * 等运算符？
type Point =
  { x :: Number
  , y :: Number
  }

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
showShape (Clipped _ _ _ _) =
  "Clipped"

showPoint :: Point -> String
showPoint { x, y } =
  "(" <> show x <> ", " <> show y <> ")"

origin :: Point
origin = { x, y }
  where
  x = 0.0
  y = 0.0

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

getCenter :: Shape -> Point
getCenter (Circle c _) = c
getCenter (Rectangle c _ _) = c
getCenter (Line s e) = (s + e) * { x: 0.5, y: 0.5 }
getCenter (Text loc _) = loc
getCenter _ = origin

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ w h) = Rectangle origin w h
centerShape (Text _ text) = Text origin text
centerShape line@(Line s e) = Line (s - delta) (e - delta)
  where
  delta = getCenter line
centerShape other = other

scaleShape :: Number -> Shape -> Shape
scaleShape i (Circle c r) = Circle c (r * i)
scaleShape i (Rectangle c w h) = Rectangle c (w * i) (h * i)
scaleShape i (Line s e) = Line (s * scale) (e * scale)
  where
  scale = { x: i, y: i }
scaleShape _ other = other

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

type Bounds =
  { top :: Number
  , left :: Number
  , bottom :: Number
  , right :: Number
  }

-- 这个定义右 union 而来，union x empty = x
emptyBounds :: Bounds
emptyBounds = { top: infinity, left: infinity, bottom: -infinity, right: -infinity }

-- 不断求 union
bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

union :: Bounds -> Bounds -> Bounds
union b1 b2 =
  { top: min b1.top b2.top
  , left: min b1.left b2.left
  , bottom: max b1.bottom b2.bottom
  , right: max b1.right b2.right
  }

intersect :: Bounds -> Bounds -> Bounds
intersect b1 b2 =
  { top: max b1.top b2.top
  , left: max b1.left b2.left
  , bottom: min b1.bottom b2.bottom
  , right: min b1.right b2.right
  }

-- 类似于从屏幕左上角开始计算
shapeBounds :: Shape -> Bounds
shapeBounds (Circle { x, y } r) =
  { top: y - r
  , left: x - r
  , bottom: y + r
  , right: x + r
  }
shapeBounds (Rectangle { x, y } w h) =
  { top: y - h / 2.0
  , left: x - w / 2.0
  , bottom: y + h / 2.0
  , right: x + w / 2.0
  }
shapeBounds (Line p1 p2) =
  { top: min p1.y p2.y
  , left: min p1.x p2.x
  , bottom: max p1.y p2.y
  , right: max p1.x p2.x
  }
shapeBounds (Text { x, y } _) = { top: y, left: x, right: x, bottom: y }
-- 剪裁就是划一块正方形
shapeBounds (Clipped picture point w h) = intersect (bounds picture) (shapeBounds (Rectangle point w h))

area :: Shape -> Number
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area (Line _ _) = 0.0
area _ = 0.0

shapeText :: Shape -> Maybe String
shapeText (Text _ str) = Just str
shapeText _ = Nothing

derive instance shapeEq :: Eq Shape

instance shapeShow :: Show Shape where
  show shape = showShape shape