{-# OPTIONS_GHC -fno-warn-tabs #-}

module Definitions (module Definitions, module Parser) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.IORef
import           Data.Maybe
import           Parser

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
	| Closure {
		cargs :: [Name],
		cbody :: Expr,
		cenv  :: LispEnvironment
	}
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
	| Lambda [Name] Expr
	| DefVal Name Expr
	| DefFun Name [Name] Expr
	| DefExpr Expr deriving (Eq, Show)

data LispError =
	Default String
	| UnboundVar String Name
	| ReservedVar String Name
	| BadExpr String deriving (Eq, Show)

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

instance Eq LispObject where
	Integral a    == Integral b    = a == b
	Floating a    == Floating b    = a == b
	Boolean a     == Boolean b     = a == b
	Symbol a      == Symbol b      = a == b
	List a        == List b        = a == b
	Primitive a _ == Primitive b _ = a == b
	Closure a b _ == Closure c d _ = a == c && b == d
	Nil           == Nil           = True
	_             == _             = False

instance Ord LispObject where
	Integral a    `compare` Integral b    = a `compare` b
	Floating a    `compare` Floating b    = a `compare` b
	Boolean a     `compare` Boolean b     = a `compare` b
	Symbol a      `compare` Symbol b      = a `compare` b
	List a        `compare` List b        = a `compare` b
	Primitive a _ `compare` Primitive b _ = a `compare` b
	Nil           `compare` Nil           = EQ
	_             `compare` _             = EQ

instance Show LispObject where
	show (Integral a)    = "Integral " ++ show a
	show (Floating a)    = "Floating " ++ show a
	show (Boolean a)     = "Boolean " ++ show a
	show (Symbol a)      = "Symbol " ++ show a
	show (Quote a)       = "Quote " ++ show a
	show (List a)        = "List " ++ show a
	show (Primitive a _) = "Primitive " ++ show a
	show (Closure a _ _) = "Closure " ++ show a
	show Nil             = "Nil"

-- Parsers for LispObject
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
isBound e n = (isJust . lookup n) <$> readIORef e

getVar :: LispEnvironment -> Name -> IOThrowsError LispObject
getVar e n  =  do
	env <- liftIO $ readIORef e
	maybe (throwError $ UnboundVar "Variable not bound" n)
		(liftIO . readIORef)
		(lookup n env)

bindVar :: LispEnvironment -> (Name, LispObject) -> IOThrowsError LispObject
bindVar envRef (var, value) = do
	aPrim <- liftIO $ isStdPrim var
	if aPrim
		then throwError $ ReservedVar "Cannot bind to primitive" var
		else do
			alreadyDefined <- liftIO $ isBound envRef var
			liftIO $ if alreadyDefined
				then do
					env <- readIORef envRef
					maybe mempty (flip writeIORef value) (lookup var env)
					return value
				else do
					env <- readIORef envRef
					valueRef <- newIORef value
					writeIORef envRef ((var, valueRef) : env)
					return value

bindVars :: LispEnvironment -> [(Name, LispObject)] -> IO LispEnvironment
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
	where
		extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
		addBinding (var, value) = do { ref <- newIORef value; return (var, ref) }

copyEnv :: LispEnvironment -> IO LispEnvironment
copyEnv e = readIORef e >>= mapM g >>= newIORef
	where g (var, ref) = readIORef ref >>= newIORef >>= (\v -> return (var, v))

envToList :: LispEnvironment -> IO LispObject
envToList e = List <$> (readIORef e >>= mapM g)
	where g (v, k) = do { ref <- readIORef k; return $ List [Symbol v, ref] }

isStdPrim :: Name -> IO Bool
isStdPrim n = return . isJust $ lookup n stdPrims

stdPrims :: [(Name, LispObject)]
stdPrims = map (\(n, f) -> (n, Primitive n f)) [
			("eq", equals),
			("neq", not_equals),
			("+", plus),
			("-", minus),
			("*", times),
			("/", divides),
			("<", lower_than),
			(">", greater_than),
			("cons", cons),
			("atom?", atom),
			("car", car),
			("cdr", cdr)
		]
	where
		plus l = case l of
			[Integral a, Integral b] -> Integral (a+b)
			[Floating a, Floating b] -> Floating (a+b)
			[Floating a, Integral b] -> Floating (a+fromIntegral b)
			[Integral a, Floating b] -> Floating (fromIntegral a+b)
			_                        -> error "(plus a b)"
		minus l = case l of
			[Integral a, Integral b] -> Integral (a-b)
			[Floating a, Floating b] -> Floating (a-b)
			[Floating a, Integral b] -> Floating (a-fromIntegral b)
			[Integral a, Floating b] -> Floating (fromIntegral a-b)
			_                        -> error "(minus a b)"
		times l = case l of
			[Integral a, Integral b] -> Integral (a*b)
			[Floating a, Floating b] -> Floating (a*b)
			[Floating a, Integral b] -> Floating (a*fromIntegral b)
			[Integral a, Floating b] -> Floating (fromIntegral a*b)
			_                        -> error "(times a b)"
		divides l = case l of
			[Integral a, Integral b] -> Integral (quot a b)
			[Floating a, Floating b] -> Floating (a/b)
			[Floating a, Integral b] -> Floating (a/fromIntegral b)
			[Integral a, Floating b] -> Floating (fromIntegral a/b)
			_                        -> error "(divides a b)"
		equals l = case l of
			[a, b] -> Boolean $ a == b
			_      -> error "(eq bool bool)"
		not_equals l = case l of
			[a, b] -> Boolean $ a /= b
			_      -> error "(neq bool bool)"
		lower_than l = case l of
			[Integral a, Integral b] -> Boolean $ a < b
			[Floating a, Floating b] -> Boolean $ a < b
			[Symbol a, Symbol b]     -> Boolean $ a < b
			[List a, List b]         -> Boolean $ a < b
			[Quote a, Quote b]       -> Boolean $ a < b
			_                        -> error "(lower_than ord ord)"
		greater_than l = case l of
			[Integral a, Integral b] -> Boolean $ a > b
			[Floating a, Floating b] -> Boolean $ a > b
			[Symbol a, Symbol b]     -> Boolean $ a > b
			[List a, List b]         -> Boolean $ a > b
			[Quote a, Quote b]       -> Boolean $ a > b
			_                        -> error "(lower_than ord ord)"
		cons l = case l of
			[a, List b] -> List $ a : b
			[a, b]      -> List [a, b]
			_           -> error "(cons atom list|atom)"
		atom l = case l of
			[List []] -> Boolean True
			[List _]  -> Boolean False
			_		  -> Boolean True
		car l = case l of
			[List (a:_)] -> a
			_          -> error "(car list)"
		cdr l = case l of
			[List (_:as)] -> List as
			_           -> error "(cdr list)"

basis :: IO LispEnvironment
basis = emptyEnv >>= flip bindVars stdPrims
