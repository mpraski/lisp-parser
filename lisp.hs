import Data.Char
import Data.Monoid
import Control.Monad
import Control.Applicative

-- Parser definitions
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    f `fmap` p = Parser $ \s -> case parse p s of
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

char :: Char -> Parser Char
char c = satisfy (c==)

digit :: Parser Char
digit = satisfy $ flip elem ['0'..'9']

lower :: Parser Char
lower = satisfy $ flip elem ['a'..'z']

upper :: Parser Char
upper = satisfy $ flip elem ['A'..'Z']

whitespace :: Parser Char
whitespace = satisfy $ flip elem [' ', '\n', '\t']

natural :: Parser Integer
natural = read <$> some digit

floating :: Parser Double
floating = do
    a <- some digit
    b <- reserved "."
    c <- some digit
    return $ read $ a ++ b ++ c

boolean :: Parser Bool
boolean = read <$> capitalize <$> (reserved "true" <|> reserved "false")

string :: String -> Parser String
string []     = return []
string (a:as) = do { char a; string as; return (a:as) }

spaces :: Parser String
spaces = many $ whitespace

token :: Parser a -> Parser a
token p = do {a <- p; spaces; return a}

reserved :: String -> Parser String
reserved s = token (string s)

parens :: Parser a -> Parser a
parens m = do
    reserved "("
    n <- m
    reserved ")"
    return n

-- Lisp object definitions
capitalize :: String -> String
capitalize (a:as) = toUpper a : map toLower as
capitalize []     = []

data Object =
    Integral Int
    | Boolean Bool
    | Symbol String
    | Pair Object Object
    | Nil deriving (Eq, Show)