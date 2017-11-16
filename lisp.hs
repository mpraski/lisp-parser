import Data.Char
import Data.Monoid
import Control.Monad
import Control.Applicative

-- Parser definitions
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f p = Parser $ \s -> case parse p s of
                                 Nothing      -> Nothing
                                 Just (a, s') -> Just (f a, s')

instance Applicative Parser where
    pure a  = Parser $ \s -> Just (a, s)
    f <*> p = Parser $ \s -> case parse f s of
                                Nothing      -> Nothing
                                Just (g, s') -> case parse p s' of
                                                   Nothing       -> Nothing
                                                   Just (a, s'') -> Just (g a, s'')

instance Monad Parser where
    return = pure
    p >>= f  = Parser $ \s -> case parse p s of
                                Nothing      -> Nothing
                                Just (a, s') -> parse (f a) s'

instance MonadPlus Parser where
    mzero       = Parser $ \s -> Nothing
    a `mplus` b = Parser $ \s -> case parse a s of
                                    Nothing      -> parse b s
                                    Just (c, s') -> Just (c, s')

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

consume :: Parser Char
consume  = Parser $ \s -> case s of
                             []     -> Nothing
                             (a:as) -> Just (a, as)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = consume >>= (\c -> if f c then return c else empty)

digit :: Parser Char
digit = satisfy $ flip elem ['0'..'9']

lower :: Parser Char
lower = satisfy $ flip elem ['a'..'z']

upper :: Parser Char
upper = satisfy $ flip elem ['A'..'Z']

whitespace :: Parser Char
whitespace = satisfy $ flip elem [' ', '\n', '\t']

-- Lisp object definitions
data Object =
    Integral Int
    | Boolean Bool
    | Symbol String
    | Pair Object Object
    | Nil deriving (Eq, Show)