module Lib
    ( someFunc
    ) where
someFunc :: IO ()
someFunc = putStrLn "foo"

foo :: IO ()
foo = someFunc
