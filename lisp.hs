import Data.Char
import Control.Monad
import Control.Applicative

-- Parser definitions
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    f `fmap` p = Parser $ \s ->
        case parse p s of
            Nothing      -> Nothing
            Just (a, s') -> Just (f a, s')

instance Applicative Parser where
    pure a  = Parser $ \s -> Just (a, s)
    f <*> p = Parser $ \s ->
        case parse f s of
            Nothing      -> Nothing
            Just (g, s') ->
                case parse p s' of
                    Nothing       -> Nothing
                    Just (a, s'') -> Just (g a, s'')

instance Monad Parser where
    return = pure
    p >>= f  = Parser $ \s ->
        case parse p s of
            Nothing      -> Nothing
            Just (a, s') -> parse (f a) s'

instance MonadPlus Parser where
    mzero       = Parser $ \s -> Nothing
    a `mplus` b = Parser $ \s ->
        case parse a s of
            Nothing      -> parse b s
            Just (c, s') -> Just (c, s')

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

run :: Parser a -> String -> a
run p s = case parse p s of
             Just(a, s') -> a
             Nothing     -> error "Parser did not succeed"

consume :: Parser Char
consume  = Parser $ \s ->
    case s of
        []     -> Nothing
        (a:as) -> Just (a, as)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = consume >>= (\c -> if f c then return c else empty)

char :: Char -> Parser Char
char c = satisfy (c==)

chars :: [Char] -> Parser Char
chars cs = satisfy $ flip elem cs

digit :: Parser Char
digit = chars ['0'..'9']

lower :: Parser Char
lower = chars ['a'..'z']

upper :: Parser Char
upper = chars ['A'..'Z']

allowed :: Parser Char
allowed = chars $ ['a'..'z'] ++ ['A'..'Z']

whitespace :: Parser Char
whitespace = chars [' ', '\n', '\t']

integral :: Parser Integer
integral = do
    s <- string "-" <|> return ""
    a <- some digit
    return $ read $ s ++ a

floating :: Parser Double
floating = do
    s <- string "-" <|> return ""
    a <- some digit <|> return "0"
    b <- reserved "."
    c <- some digit
    return $ read $ s ++ a ++ b ++ c

boolean :: Parser Bool
boolean = do
    a <- reserved "true" <|> reserved "false"
    return $
        case a of
            "true"  -> True
            "false" -> False


string :: String -> Parser String
string []     = return []
string (a:as) = do { char a; string as; return (a:as) }

anyString :: Parser String
anyString = token (some allowed)

spaces :: Parser String
spaces = many $ whitespace

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved s = token (string s)

-- Lisp specific definitions
data LispObject =
    Integral Integer
    | Floating Double
    | Boolean Bool
    | Symbol String
    | Pair LispObject LispObject
    | Nil deriving (Eq, Show)

lispObject :: Parser LispObject
lispObject = lispFloating
    <|> lispIntegral
    <|> lispBoolean
    <|> lispNil
    <|> lispSymbol
    <|> lispList

lispIntegral :: Parser LispObject
lispIntegral = do { a <- integral; return (Integral a)}

lispFloating :: Parser LispObject
lispFloating = do { a <- floating; return (Floating a)}

lispSymbol :: Parser LispObject
lispSymbol = do { a <- anyString; return (Symbol a)}

lispBoolean :: Parser LispObject
lispBoolean = do { a <- boolean; return (Boolean a)}

lispNil :: Parser LispObject
lispNil = do { reserved "nil"; return Nil}

lispList :: Parser LispObject
lispList = do
    reserved "("
    a <- lispObject
    spaces
    b <- lispObject
    reserved ")"
    return (Pair a b)