{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import           Control.Applicative
import           Control.Monad

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
	mzero       = Parser $ const Nothing
	a `mplus` b = Parser $ \s ->
		case parse a s of
			Nothing      -> parse b s
			Just (c, s') -> Just (c, s')

instance Alternative Parser where
	empty = mzero
	(<|>) = mplus

run :: Parser a -> String -> a
run p s =
	case parse p s of
		Just (a, _) -> a
		Nothing     -> error "    Parser did not succeed"

consume :: Parser Char
consume = Parser $ \case
	[]     -> Nothing
	(a:as) -> Just (a, as)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = consume >>= (\c -> if f c then return c else empty)

char :: Char -> Parser Char
char c = satisfy (c==)

chars :: String -> Parser Char
chars cs = satisfy $ flip elem cs

digit :: Parser Char
digit = chars ['0'..'9']

lower :: Parser Char
lower = chars ['a'..'z']

upper :: Parser Char
upper = chars ['A'..'Z']

symbol :: Parser Char
symbol = chars $ ['a'..'z'] ++ ['A'..'Z'] ++ ['*','/','>','<','=','?','!','-','+']

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

spaces :: Parser String
spaces = many whitespace

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved s = token (string s)
