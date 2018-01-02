{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char
import Control.Monad
import Control.Applicative
import System.IO

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
run p s =
	case parse p s of
		Just (a, _) -> a
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

-- Lisp specific definitions
type LispFunc = [LispObject] -> LispObject

data LispObject =
	Integral Integer
	| Floating Double
	| Boolean Bool
	| Symbol String
	| Reserved String
	| List [LispObject]
	| Primitive String LispFunc
	| Nil

instance Eq LispObject where
	Integral a    == Integral b    = a == b
	Floating a    == Floating b    = a == b
	Boolean a     == Boolean b     = a == b
	Symbol a      == Symbol b      = a == b
	Reserved a    == Reserved b    = a == b
	List a        == List b        = a == b
	Primitive a _ == Primitive b _ = a == b
	Nil           == Nil           = True
	_			  == _			   = False

instance Show LispObject where
	show (Integral a)    = "Integral " ++ show a
	show (Floating a)    = "Floating " ++ show a
	show (Boolean a)     = "Boolean " ++ show a
	show (Symbol a)      = "Symbol " ++ show a
	show (Reserved a)    = "Reserved " ++ show a
	show (List a)        = "List " ++ show a
	show (Primitive a f) = "Primitive " ++ show a
	show Nil             = "Nil"

lispObject :: Parser LispObject
lispObject = lispFloating
	<|> lispIntegral
	<|> lispBoolean
	<|> lispNil
	<|> lispList
	<|> lispReserved
	<|> lispSymbol

lispIntegral :: Parser LispObject
lispIntegral = do { a <- integral; spaces; return $ Integral a }

lispFloating :: Parser LispObject
lispFloating = do { a <- floating; spaces; return $ Floating a }

lispReserved :: Parser LispObject
lispReserved = do
	a <- reserved "if"
		<|> reserved "val"
		<|> reserved "env"
	return $ Reserved a

lispSymbol :: Parser LispObject
lispSymbol = do { a <- token (some symbol); return $ Symbol a }

lispBoolean :: Parser LispObject
lispBoolean = do { a <- boolean; spaces; return $ Boolean a }

lispNil :: Parser LispObject
lispNil = do { reserved "nil"; return Nil }

lispList :: Parser LispObject
lispList = do
	reserved "("
	a <- many lispObject
	reserved ")"
	return $ List a

-- Define environment
type LispEnvironment = [(String, LispObject)]

bind :: (String, LispObject) -> LispEnvironment -> LispEnvironment
bind (s, o) ((s', o') : e) = if s == s' then (s, o) : e else (s', o') : (bind (s, o) e)
bind (s, o) []             = [(s, o)]

envToList :: LispEnvironment -> LispObject
envToList e = List $ foldl (\acc (k, v) -> (List [Symbol k, v]) : acc) [] e

-- Evaluate the input
eval :: LispObject -> LispEnvironment -> (LispObject, LispEnvironment)

eval (Primitive n f) e = (Primitive n f, e)

eval (List l) e = case l of
	(Reserved "if"):c:t:f:[]         -> (eval_if c t f e, e)
	(Reserved "val"):(Symbol s):o:[] -> let (o', _) = eval o e in (o', bind (s, o') e)
	(Symbol s):as                    -> case fst $ eval (Symbol s) e of
											(Primitive n f) -> (f (eval_args as e), e)
											_               -> error "(apply func args)"
	as							 	 -> (List as, e)
	where
		eval_if c t f e =
			case fst $ eval c e of
				(Boolean b) -> if b then t else f
				_           -> error "(if bool true false)"
		eval_args as e = foldr (\x acc -> (fst $ eval x e) : acc) [] as

eval (Reserved "env") e = (envToList e, e)

eval (Integral i) e = (Integral i, e)
eval (Floating f) e = (Floating f, e)
eval (Boolean b) e  = (Boolean b, e)
eval (Reserved r) e = (Reserved r, e)

eval (Symbol s) e   =
	case lookup s e of
		Just o  -> (o, e)
		Nothing -> (Symbol s, e)

eval Nil e          = (Nil, e)

addPrim :: LispEnvironment -> (String, LispFunc) -> LispEnvironment
addPrim e (n, f) = bind (n, Primitive n f) e

basis :: LispEnvironment
basis = foldl addPrim [] [
		("eq", equals),
		("neq", not_equals),
		("+", plus),
		("-", minus),
		("*", times),
		("/", divides),
		("list", list)
	]
	where
		plus l = case l of
			(Integral a):(Integral b):[] -> Integral (a+b)
			(Floating a):(Floating b):[] -> Floating (a+b)
			(Floating a):(Integral b):[] -> Floating (a+(fromIntegral b))
			(Integral a):(Floating b):[] -> Floating ((fromIntegral a)+b)
			_                            -> error "(plus a b)"
		minus l = case l of
			(Integral a):(Integral b):[] -> Integral (a-b)
			(Floating a):(Floating b):[] -> Floating (a-b)
			(Floating a):(Integral b):[] -> Floating (a-(fromIntegral b))
			(Integral a):(Floating b):[] -> Floating ((fromIntegral a)-b)
			_                            -> error "(minus a b)"
		times l = case l of
			(Integral a):(Integral b):[] -> Integral (a*b)
			(Floating a):(Floating b):[] -> Floating (a*b)
			(Floating a):(Integral b):[] -> Floating (a*(fromIntegral b))
			(Integral a):(Floating b):[] -> Floating ((fromIntegral a)*b)
			_                            -> error "(times a b)"
		divides l = case l of
			(Integral a):(Integral b):[] -> Integral (quot a b)
			(Floating a):(Floating b):[] -> Floating (a/b)
			(Floating a):(Integral b):[] -> Floating (a/(fromIntegral b))
			(Integral a):(Floating b):[] -> Floating ((fromIntegral a)/b)
			_                            -> error "(divides a b)"
		equals l = case l of
			a:b:[] -> Boolean $ a == b
		not_equals l = case l of
			a:b:[] -> Boolean $ a /= b
		list l = List l

-- Define the repl function
repl :: IO String
repl = do
	hSetBuffering stdin LineBuffering
	r basis
	where r e = do
		putChar '>'
		i <- getLine
		case i of
			"/q" -> return "goodbye"
			_    -> do
				let (o, e') = eval (run lispObject i) e
				putStrLn $ show o
				r e'