{-# OPTIONS_GHC -fno-warn-tabs #-}

module Evaluator (module Evaluator, module Definitions) where

import           Control.Monad.Except
import           Definitions

-- build and evaluate AST
buildExpr :: LispObject -> Expr
buildExpr (Primitive _ _)   = error "Not expected"
buildExpr (IOPrimitive _ _) = error "Not expected"
buildExpr (Closure _ _ _)   = error "Not expected"
buildExpr (Integral i)      = Literal (Integral i)
buildExpr (Floating f)      = Literal (Floating f)
buildExpr (Boolean b)       = Literal (Boolean b)
buildExpr (String s)        = Literal (String s)
buildExpr (Quote q)         = Literal (Quote q)
buildExpr Nil               = Literal Nil
buildExpr (Symbol s)        = Var s
buildExpr (List l)          =
	case l of
		[Symbol "if",     c, t, f]                   -> If (buildExpr c) (buildExpr t) (buildExpr f)
		[Symbol "and",    a, b]                      -> And (buildExpr a) (buildExpr b)
		[Symbol "or",     a, b]                      -> Or (buildExpr a) (buildExpr b)
		[Symbol "val",    Symbol s, o]               -> DefVal s (buildExpr o)
		[Symbol "lambda", List args, body]           -> Lambda (get_args <$> args) (buildExpr body)
		[Symbol "define", Symbol n, List args, body] -> DefFun n (get_args <$> args) (buildExpr body)
		[Symbol "apply",  Symbol fn, args]           -> ApplyOne (buildExpr $ Symbol fn) (buildExpr args)
		Symbol fn : args                             -> Apply (buildExpr $ Symbol fn) (buildExpr <$> args)
		[]                                           -> Literal $ List []
		_                                            -> error "Poorly formed expression"
	where get_args a = case a of
		Symbol s -> s
		_        -> error "(lambda (args) body)"

evalExpr :: Expr -> LispEnvironment -> IOThrowsError LispObject
evalExpr (Literal (Quote q)) _ = return q
evalExpr (Literal l) _ = return l
evalExpr (Lambda a b) e = return Closure { cargs=a, cbody=b, cenv=e }
evalExpr (Var "env") e = liftIO $ envToList e
evalExpr (Var n) e = getVar e n
evalExpr (If b t f) e = do
	c <- evalExpr b e
	flip evalExpr e $ if c == Boolean True then t else f

evalExpr (And a b) e = do
	r1 <- evalExpr a e
	r2 <- evalExpr b e
	case (r1, r2) of
		(Boolean c, Boolean d) -> return $ Boolean (c && d)
		_                      -> throwError $ BadExpr "(and bool bool)"
evalExpr (Or a b) e = do
	r1 <- evalExpr a e
	r2 <- evalExpr b e
	case (r1, r2) of
		(Boolean c, Boolean d) -> return $ Boolean (c || d)
		_                      -> throwError $ BadExpr "(or bool bool)"
evalExpr (Apply fn args) e = do
	f <- evalExpr fn e
	as <- mapM (flip evalExpr e) args
	case f of
		Primitive _ f   -> return $ f as
		IOPrimitive _ f -> f as
		Closure c b e   -> (liftIO . bindVars e $ zip c as) >>= evalExpr b
		_               -> throwError $ BadExpr "(apply func args)"

evalExpr (ApplyOne fn args) e = do
	f <- evalExpr fn e
	as <- evalExpr args e
	case (f, as) of
		(Primitive _ f, List ass)   -> return $ f ass
		(Primitive _ f, o)          -> return $ f [o]
		(IOPrimitive _ f, List ass) -> f ass
		(IOPrimitive _ f, o)        -> f [o]
		(Closure c b e, List ass)   -> (liftIO . bindVars e $ zip c ass) >>= evalExpr b
		(Closure c b e, o)          -> (liftIO . bindVars e $ zip c [o]) >>= evalExpr b
		_                           -> throwError $ BadExpr "(apply func args)"

evalExpr (DefVal n v) e = evalExpr v e >>= (\va -> bindVar e (n, va))

evalExpr (DefFun n args body) e = do
	ev <- evalExpr (Lambda args body) e
	case ev of
		Closure a b env -> do
			newEnv <- liftIO $ copyEnv env
			clo <- liftIO . return $ Closure a b newEnv
			bindVar e (n, clo)
			return clo
		_               -> throwError $ BadExpr "Expecting closure"

evalExpr (DefExpr v) e = evalExpr v e
