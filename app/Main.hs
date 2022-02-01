module Main where

import Complete

main :: IO ()
main = do
    putStrLn "This program does not do much, but take a look the source in /app or the blog post at https://hugopeters.me/posts/16"
    putStrLn "check this out however:"
    print $ reduce $ intToLambda 10
