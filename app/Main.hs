module Main where

import qualified Complete as C
import qualified Scoped as S

main :: IO ()
main = do
    putStrLn "This program does not do much, but take a look the source in /app or the blog post at https://hugopeters.me/posts/16"
    putStrLn "check this out however:"
    putStrLn $ "10 == " <> show (S.reduce $ S.intToLambda 10)
    putStrLn $ "mockingbird: " <> show S.mockingbird
    putStrLn $ "ycombinator: " <> show S.ycombinator





