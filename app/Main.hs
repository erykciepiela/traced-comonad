module Main where

import Control.Comonad
import Data.Monoid
import Control.Monad.Fix

data Point = Point {
    px :: Float,
    py :: Float
} deriving Show

instance Semigroup Point where
    (Point x1 y1) <> (Point x2 y2) = Point (x1 + x2) (y1 + y2)

instance Monoid Point where
    mempty = Point 0 0 

data VP = VP { -- view point
    op :: Point,
    opa :: Float -- angle
} deriving Show

instance Semigroup VP where
    (VP (Point x1 y1) a1) <> (VP (Point x2 y2) a2) = VP (Point (x1 + cos a1 * x2 - sin a1 * y2) (y1 + sin a1 * x2 + cos a1 * y2)) (a1 + a2)

instance Monoid VP where
    mempty = VP mempty 0

point :: (VP -> VP) -> Point
point f = op $ f mempty

angle :: (VP -> VP) -> Float
angle f = opa $ f mempty

-- combinators
rotate :: Float -> (VP -> w) -> w
rotate angle f = f (VP mempty angle)

translate :: Point -> (VP -> w) -> w
translate p f = f (VP p 0) 

-- primitive shapes
data Rect = Rect {
    rectP1 :: Point,
    rectP2 :: Point
} deriving Show

data Line = Line {
    lineP1 :: Point,
    lineP2 :: Point
} deriving Show

data Circle = Circle {
    circleCenter :: Point,
    circleRadius :: Float
} deriving Show


hline :: Float -> (VP -> Point) -> Line
hline l f = Line (translate (Point (-l/2) 0) f) (translate (Point (l/2) 0) f)

vline :: Float -> (VP -> Point) -> Line
vline l f = Line (translate (Point 0 (-l/2)) f) (translate (Point 0 (l/2)) f)

line :: Point -> Point -> (VP -> Point) -> Line
line a b f = Line (translate a f) (translate b f)

rect :: Float -> Float -> (VP -> Point) -> Rect
rect w h f = Rect (translate (Point (-w/2) (-h/2)) f) (translate (Point (w/2) (h/2)) f)

circle :: Float -> (VP -> Point) -> Circle
circle radius f = Circle (f mempty) radius

-- derived shapes
rectAndCircle :: (VP -> VP) -> (Rect, Circle, Line)
rectAndCircle f = let
    rc = Point 1 0
    r = (point =>= translate rc =>= rect 10 6) f
    cc = Point 10 0
    c = (point =>= translate cc =>= circle 5) f
    l = (point =>= line rc cc) f
    in (r, c, l)

oncircle :: Float -> (VP -> w) -> [a] -> [(w, a)]
oncircle r f as = let 
    n = length as 
    in zip ((\angle -> (rotate angle =>= translate (Point r 0)) f) <$> iterate ((2 * pi / fromIntegral (length as)) +) 0) as

main :: IO ()
main = print $ rectAndCircle id
