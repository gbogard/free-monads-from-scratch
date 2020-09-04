module Lib where

newtype Musician = Musician String
play :: [Musician] -> IO ()
play _ = pure ()

a = Musician "John"
b = Musician "Paul"
c = Musician "Ringo"
d = Musician "George"

song = play [a, b, c, d]



