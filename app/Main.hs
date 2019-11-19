module Main where

import Control.Comonad

x :: ([String] -> String) -> String
x f = f ["x"]

y :: ([String] -> String) -> String
y f = f ["y"]

z :: ([String] -> String) -> String
z f = f ["z"]

main :: IO ()
main = do
    putStrLn $ x concat -- yields: x
    putStrLn $ (x =>= y) concat -- yields: yx
    putStrLn $ (x =>= y =>= z) concat -- yields: zyx
