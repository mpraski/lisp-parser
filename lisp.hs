{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char
import Data.IORef
import Control.Monad
import Control.Applicative
import Control.Monad.Except
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
type Name = String

data LispObject =
	Integral Integer
	| Floating Double
	| Boolean Bool
	| Symbol Name
	| List [LispObject]
	| Primitive Name LispFunc
	| Quote Value
	| Nil

type Value = LispObject

data Expr =
	Literal Value
	| Var Name
	| If Expr Expr Expr
	| And Expr Expr
	| Or Expr Expr
	| Apply Expr [Expr]
	| ApplyOne Expr Expr
	| DefVal Name Expr
	| DefExpr Expr deriving (Eq, Show)

data LispError =
	Default String
	| UnboundVar String Name
	| BadExpr String deriving (Eq, Show)

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

instance Eq LispObject where
	Integral a    == Integral b    = a == b
	Floating a    == Floating b    = a == b
	Boolean a     == Boolean b     = a == b
	Symbol a      == Symbol b      = a == b
	List a        == List b        = a == b
	Primitive a _ == Primitive b _ = a == b
	Nil           == Nil           = True
	_			  == _			   = False

instance Ord LispObject where
	Integral a    `compare` Integral b    = a `compare` b
	Floating a    `compare` Floating b    = a `compare` b
	Boolean a     `compare` Boolean b     = a `compare` b
	Symbol a      `compare` Symbol b      = a `compare` b
	List a        `compare` List b        = a `compare` b
	Primitive a _ `compare` Primitive b _ = a `compare` b
	Nil           `compare` Nil           = EQ

instance Show LispObject where
	show (Integral a)    = "Integral " ++ show a
	show (Floating a)    = "Floating " ++ show a
	show (Boolean a)     = "Boolean " ++ show a
	show (Symbol a)      = "Symbol " ++ show a
	show (Quote a)    	 = "Quote " ++ show a
	show (List a)        = "List " ++ show a
	show (Primitive a f) = "Primitive " ++ show a
	show Nil             = "Nil"

lispObject :: Parser LispObject
lispObject = lispFloating
	<|> lispIntegral
	<|> lispBoolean
	<|> lispNil
	<|> lispList
	<|> lispSymbol
	<|> lispQuote

lispIntegral :: Parser LispObject
lispIntegral = do { a <- integral; spaces; return $ Integral a }

lispFloating :: Parser LispObject
lispFloating = do { a <- floating; spaces; return $ Floating a }

lispSymbol :: Parser LispObject
lispSymbol = do { a <- token (some symbol); return $ Symbol a }

lispQuote :: Parser LispObject
lispQuote = do
	reserved "'"
	a <- lispObject
	return $ Quote a

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
type LispEnvironment = IORef [(Name, IORef LispObject)]

emptyEnv :: IO LispEnvironment
emptyEnv = newIORef []

isBound :: LispEnvironment -> Name -> IO Bool
isBound e n = readIORef e >>= return . maybe False (const True) . lookup n

getVar :: LispEnvironment -> Name -> IOThrowsError LispObject
getVar e n  =  do
	env <- liftIO $ readIORef e
	maybe (throwError $ UnboundVar "Getting an unbound variable" n)
		(liftIO . readIORef)
		(lookup n env)

bindVar :: LispEnvironment -> (Name, LispObject) -> IOThrowsError LispObject
bindVar envRef (var, value) = do
     alreadyDefined <- liftIO $ isBound envRef var
     liftIO $ if alreadyDefined
        then do
        	env <- readIORef envRef
        	maybe (return ()) (liftIO . (flip writeIORef value)) (lookup var env)
        	return value
 		else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: LispEnvironment -> [(Name, LispObject)] -> IO LispEnvironment
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

envToList :: LispEnvironment -> IO LispObject
envToList e = readIORef e >>= toList
	where
		toList env = (mapM flatten env) >>= (\xs -> return $ List xs)
		flatten (var, value) = do
			ref <- readIORef value;
			return $ List [Symbol var, ref]

basis :: IO LispEnvironment
basis = emptyEnv >>= (flip bindVars decl)
	where
		decl = map (\(n, f) -> (n, Primitive n f)) [
			("eq", equals),
			("neq", not_equals),
			("+", plus),
			("-", minus),
			("*", times),
			("/", divides),
			("<", lower_than),
			(">", greater_than)]
		plus l = case l of
			[Integral a, Integral b] -> Integral (a+b)
			[Floating a, Floating b] -> Floating (a+b)
			[Floating a, Integral b] -> Floating (a+(fromIntegral b))
			[Integral a, Floating b] -> Floating ((fromIntegral a)+b)
			_                            -> error "(plus a b)"
		minus l = case l of
			[Integral a, Integral b] -> Integral (a-b)
			[Floating a, Floating b] -> Floating (a-b)
			[Floating a, Integral b] -> Floating (a-(fromIntegral b))
			[Integral a, Floating b] -> Floating ((fromIntegral a)-b)
			_                            -> error "(minus a b)"
		times l = case l of
			[Integral a, Integral b] -> Integral (a*b)
			[Floating a, Floating b] -> Floating (a*b)
			[Floating a, Integral b] -> Floating (a*(fromIntegral b))
			[Integral a, Floating b] -> Floating ((fromIntegral a)*b)
			_                            -> error "(times a b)"
		divides l = case l of
			[Integral a, Integral b] -> Integral (quot a b)
			[Floating a, Floating b] -> Floating (a/b)
			[Floating a, Integral b] -> Floating (a/(fromIntegral b))
			[Integral a, Floating b] -> Floating ((fromIntegral a)/b)
			_                            -> error "(divides a b)"
		equals l = case l of
			[a, b] -> Boolean $ a == b
		not_equals l = case l of
			[a, b] -> Boolean $ a /= b
		lower_than l = case l of
			[Integral a, Integral b] -> Boolean $ a < b
			[Floating a, Floating b] -> Boolean $ a < b
			[Symbol a, Symbol b] 	 -> Boolean $ a < b
			[List a, List b] 		 -> Boolean $ a < b
			[Quote a, Quote b] 		 -> Boolean $ a < b
			_                        -> error "(lower_than obj obj)"
		greater_than l = case l of
			[Integral a, Integral b] -> Boolean $ a > b
			[Floating a, Floating b] -> Boolean $ a > b
			[Symbol a, Symbol b] 	 -> Boolean $ a > b
			[List a, List b] 		 -> Boolean $ a > b
			[Quote a, Quote b] 		 -> Boolean $ a > b
			_                        -> error "(lower_than obj obj)"

-- build and evaluate AST
buildExpr :: LispObject -> Expr
buildExpr (Primitive n f) = error "Not expected"
buildExpr (Integral i)    = Literal (Integral i)
buildExpr (Floating f)    = Literal (Floating f)
buildExpr (Boolean b)     = Literal (Boolean b)
buildExpr (Quote q)		  = Literal (Quote q)
buildExpr Nil			  = Literal Nil
buildExpr (Symbol s)      = Var s
buildExpr (List l)	      = case l of
	[Symbol "if", c, t, f]            -> If (buildExpr c) (buildExpr t) (buildExpr f)
	[Symbol "and", a, b]		      -> And (buildExpr a) (buildExpr b)
	[Symbol "or", a, b]		    	  -> Or (buildExpr a) (buildExpr b)
	[Symbol "def", Symbol s, o] 	  -> DefVal s (buildExpr o)
	[Symbol "apply", Symbol fn, args] -> ApplyOne (buildExpr $ Symbol fn) (buildExpr args)
	(Symbol fn):args	   			  -> Apply (buildExpr $ Symbol fn) (buildExpr <$> args)
	[]								  -> Literal $ List []
	_								  -> error "Poorly formed expression"

evalExpr :: Expr -> LispEnvironment -> IOThrowsError LispObject
evalExpr (Literal (Quote q)) e = return q
evalExpr (Literal l) e = return l
evalExpr (Var "env") e = liftIO $ envToList e
evalExpr (Var n) e = getVar e n
evalExpr (If b t f) e = do
	c <- evalExpr b e
	(flip evalExpr e) $ if c == Boolean True then t else f

evalExpr (And a b) e = do
	r1 <- evalExpr a e
	r2 <- evalExpr b e
	case (r1, r2) of
		(Boolean c, Boolean d) -> return $ Boolean (c && d)
		_				   	   -> throwError $ BadExpr "(and bool bool)"
evalExpr (Or a b) e = do
	r1 <- evalExpr a e
	r2 <- evalExpr b e
	case (r1, r2) of
		(Boolean c, Boolean d) -> return $ Boolean (c || d)
		_				   	   -> throwError $ BadExpr "(or bool bool)"
evalExpr (Apply fn args) e = do
	f <- evalExpr fn e
	as <- mapM (flip evalExpr e) args
	case f of
		Primitive _ f -> return $ f as
		_			  -> throwError $ BadExpr "(apply func args)"

evalExpr (ApplyOne fn args) e = do
	f <- evalExpr fn e
	as <- evalExpr args e
	case (f, as) of
		(Primitive _ f, List ass) -> return $ f ass
		(Primitive _ f, o) 		  -> return $ f [o]
		_			  			  -> throwError $ BadExpr "(apply func args)"

evalExpr (DefVal n v) e = do
	va <- evalExpr v e
	bindVar e (n, va)
	return va

evalExpr (DefExpr v) e = evalExpr v e

-- Define the repl function
repl :: IO ()
repl = do
	hSetBuffering stdin LineBuffering
	runRepl

runRepl :: IO ()
runRepl = basis >>= until_ (== "quit") (readPrompt "> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: LispEnvironment -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ evalExpr (buildExpr $ run lispObject expr) env

evalAndPrint :: LispEnvironment -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn