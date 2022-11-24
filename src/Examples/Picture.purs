module Examples.Picture
  ( Point
  , Shape
  , centerShape
  , circleAtOrigin
  , doubleScaleAndCenter
  , origin
  , scaleShape
  , showPoint
  , showShape
  ) where

import Prelude

-- 没有匹配到的 pattern matching 会导致 runtime error
-- 尽量写 total function 避免这种情况，应该用 Maybe 去明确地处理
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

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

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ w h) = Rectangle origin w h
centerShape (Text _ text) = Text origin text
centerShape line@(Line s e) = Line (s - delta) (e - delta)
  where
  delta = getCenter line

scaleShape :: Number -> Shape -> Shape
scaleShape i (Circle c r) = Circle c (r * i)
scaleShape i (Rectangle c w h) = Rectangle c (w * i) (h * i)
scaleShape i (Line s e) = Line (s * scale) (e * scale)
  where
  scale = { x: i, y: i }
scaleShape _ text = text

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0
