module FizzBuzz where

-- data Rule = Rule { divisor :: Int, display :: String }
-- check n (Rule k str)

-- data Rule = Rule Int String
-- rules = [ Rule 3 "Fizz"
--         , Rule 5 "Buzz"]

type Rule = (Int, String)

rules :: [Rule]
rules = [(3,"Fizz"),(5,"Buzz")]

check :: Int -> Rule -> String
check n (k, str)
    | n `mod` k == 0 = str
    | otherwise      = ""

-- If neither Fizz nor Buzz, show the number
baseCase :: Int -> String -> String
baseCase n ""  = show n
baseCase _ str = str

-- A single number
fizzBuzz :: Int -> String
fizzBuzz n = baseCase n $ check n =<< rules

-- All numbers
fizzBuzzes :: [String]
fizzBuzzes = fizzBuzz <$> [1..]

-- >>> take 15 fizzBuzzes
-- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz"]
