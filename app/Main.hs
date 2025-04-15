module Main where

data INIValue
    = INIString String
    deriving(Show, Eq)

data INIKey
    = INIKeyName String
    deriving(Show, Eq)

data INIEntry
    -- String is the section name, list is key value pairs
    = INISection String [(INIKey, INIValue)]
    -- Global values consist of a single key value pair
    | INIGlobal (INIKey, INIValue)
    deriving(Show, Eq)

type INIFile = [INIEntry]

data ParseError
    = ParseError Int String
    deriving(Show)

newtype Parser p
    = Parser
    {
        -- Either return a ParseError if parsing fails or (remaining, result) pair
        parse :: String -> Either ParseError (String, p)
    }


charParser ::  Char -> Parser Char
charParser c = Parser $ \input -> case input of
    (x:xs) | x == c -> Right(xs, x)
    (_:_)           -> Left(ParseError 1 ("Expected '" ++ [c] ++ "'"))
    []              -> Left(ParseError 1 ("Unexprected Input"))

main :: IO()
main = undefined
