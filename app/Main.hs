module Main where

import Prelude hiding ((.), id)
import Control.Comonad
import Data.Monoid
import Control.Monad.Fix
import Control.Category

-- | ViewPoint
data VP = VP {
    -- ^ x coordinate
    vpx :: Float,
    -- ^ y coordinate
    vpy :: Float,
    -- ^ angle
    vpa :: Float,
    -- ^ scale
    vps :: Float
} deriving Show

instance Semigroup VP where
    (VP x1 y1 a1 s1) <> (VP x2 y2 a2 s2) = VP (x1 + cos a1 * x2 - sin a1 * y2) (y1 + sin a1 * x2 + cos a1 * y2) (a1 + a2) (s1 * s2)

instance Monoid VP where
    mempty = VP 0 0 0 1

-- combinators
rotate :: Float -> (VP -> x) -> x
rotate angle f = f (VP 0 0 angle 1)

translate :: Float -> Float -> (VP -> x) -> x
translate x y f = f (VP x y 0 1)

-- s in [0;1]
scale :: Float -> (VP -> x) -> x
scale s f = f (VP 0 0 0 s)

-- primitive shapes
vp :: (VP -> VP) -> VP
vp f = f mempty

data Point = Point {
    px :: Float,
    py :: Float
} deriving Show

point :: (VP -> VP) -> Point
point = do
    VP x y _ _ <- vp
    return $ Point x y

data Line = Line {
    lineP1 :: Point,
    lineP2 :: Point
} deriving Show

line :: Float -> (VP -> VP) -> Line
line l = do
    p1 <- point =>= translate (-l/2) 0
    p2 <- point =>= translate (l/2) 0
    return $ Line p1 p2
line' :: Float -> (VP -> VP) -> Line
line' l f = let
    p1 = (point =>= translate (-l/2) 0) f
    p2 = (point =>= translate (l/2) 0) f
    in Line p1 p2

data Rect = Rect {
    rectP1 :: Point,
    rectP2 :: Point
} deriving Show

rect :: Float -> Float -> (VP -> VP) -> Rect
rect w h = do
    p1 <- point =>= translate (-w/2) (-h/2)
    p2 <- point =>= translate (w/2) (h/2)
    return $ Rect p1 p2

data Circle = Circle {
    circleCenter :: Point,
    circleRadius :: Float
} deriving Show


circle :: Float -> (VP -> VP) -> Circle
circle radius = do
    c <- point
    return $ Circle c radius

main :: IO ()
main = do
    print $ (rect 10 6) id
    print $ (rect 10 6 =>= translate 1 2) id
    print $ (rect 10 6 =>= rotate (pi/2)) id
    print $ (rect 10 6 =>= rotate (pi/2) =>= translate 1 2) id

data Rect4 = Rect4 Point Point Point Point

rectFromDots :: Float -> Float -> (VP -> Point) -> Rect4
rectFromDots width height points = let
    p1 = points (VP (-width/2) (-height/2) 0 1)
    p2 = points (VP (-width/2) (height/2) 0 1)
    p3 = points (VP (width/2) (height/2) 0 1)
    p4 = points (VP (width/2) (-height/2) 0 1)
    in Rect4 p1 p2 p3 p4
    