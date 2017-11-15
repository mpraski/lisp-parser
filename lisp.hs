import Data.Char
import Control.Monad

-- Parser definitions
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f p = Parser $ \s -> case parse p s of
                                Nothing -> Nothing
                                Just (a, s') -> Just (f a, s')

instance Applicative Parser where
    pure a  = Parser $ \s -> Just (a,s)
    f <*> p = Parser $ \s -> case parse f s of
                                Nothing -> Nothing
                                Just (g, s') -> case parse p s' of
                                                   Nothing -> Nothing
                                                   Just (a, s'') -> Just (g a, s'')

instance Monad Parser where
    return a = Parser $ \s -> Just (a,s)
    p >>= f  = Parser $ \s -> case parse p s of
                                Nothing -> Nothing
                                Just (a, s') -> parse (f a) s'

zero :: Parser a
zero = Parser $ \s -> Nothing

consume :: Parser Char
consume  = Parser $ \s -> case s of
                            []     -> Nothing
                            (a:as) -> Just (a, as)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = consume >>= (\c -> if f c then return c else zero)

digit :: Parser Char
digit = satisfy (\c -> elem c ['0'..'9'])

whitespace :: Parser Char
whitespace = satisfy (\c -> elem c [' ', '\n', '\t'])

-- Lisp object definitions
data Object =
    Integral Int
    | Boolean Bool
    | Symbol String
    | Pair Object Object
    | Nil deriving (Eq, Show)