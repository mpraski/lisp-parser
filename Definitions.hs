{-# OPTIONS_GHC -fno-warn-tabs #-}

module Definitions where

import           Data.IORef
import           Control.Monad.Except

-- Lisp specific definitions
type LispEnvironment = IORef [(Name, IORef LispObject)]

type LispFunc = [LispObject] -> LispObject
type IOLispFunc = [LispObject] -> LispEnvironment -> IOThrowsError LispObject

type Name = String

data LispObject =
	Integral Integer
	| Floating Double
	| Boolean Bool
	| Symbol Name
	| String String
	| List [LispObject]
	| Primitive Name LispFunc
	| IOPrimitive Name IOLispFunc
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

trapError :: (Show a, MonadError a m) => m String -> m String
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
	show (Integral a)      = "Integral " ++ show a
	show (Floating a)      = "Floating " ++ show a
	show (Boolean a)       = "Boolean " ++ show a
	show (Symbol a)        = "Symbol " ++ show a
	show (String a)        = "String " ++ show a
	show (Quote a)         = "Quote " ++ show a
	show (List a)          = "List " ++ show a
	show (Primitive a _)   = "Primitive " ++ show a
	show (IOPrimitive a _) = "IOPrimitive " ++ show a
	show (Closure a _ _)   = "Closure " ++ show a
	show Nil               = "Nil"