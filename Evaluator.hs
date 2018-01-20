{-# OPTIONS_GHC -fno-warn-tabs #-}

module Evaluator (module Evaluator, module Parser) where

import           Data.Maybe
import           Data.IORef
import           Control.Monad.Except
import           Parser

-- Environment functions
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
		extendEnv bs env = fmap (++ env) (mapM addBinding bs)
		addBinding (var, value) = do { ref <- newIORef value; return (var, ref) }

copyEnv :: LispEnvironment -> IO LispEnvironment
copyEnv e = readIORef e >>= mapM g >>= newIORef
	where g (var, ref) = readIORef ref >>= newIORef >>= (\v -> return (var, v))

envToList :: LispEnvironment -> IO LispObject
envToList e = List <$> (readIORef e >>= mapM g)
	where g (v, k) = do { ref <- readIORef k; return $ List [Symbol v, ref] }

isStdPrim :: Name -> IO Bool
isStdPrim n = return . isJust $ lookup n (stdPrims ++ stdIOPrims)

stdPrims :: [(Name, LispObject)]
stdPrims = map (\(n, f) -> (n, Primitive n f)) [
			("eq?", equals),
			("neq?", not_equals),
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
			[Integral a, Integral b] -> return $ Integral (a+b)
			[Floating a, Floating b] -> return $ Floating (a+b)
			[Floating a, Integral b] -> return $ Floating (a+fromIntegral b)
			[Integral a, Floating b] -> return $ Floating (fromIntegral a+b)
			_                        -> liftThrows $ throwError (BadArg "(plus num num)")
		minus l = case l of
			[Integral a, Integral b] -> return $ Integral (a-b)
			[Floating a, Floating b] -> return $ Floating (a-b)
			[Floating a, Integral b] -> return $ Floating (a-fromIntegral b)
			[Integral a, Floating b] -> return $ Floating (fromIntegral a-b)
			_                        -> liftThrows $ throwError (BadArg "(minus num num)")
		times l = case l of
			[Integral a, Integral b] -> return $ Integral (a*b)
			[Floating a, Floating b] -> return $ Floating (a*b)
			[Floating a, Integral b] -> return $ Floating (a*fromIntegral b)
			[Integral a, Floating b] -> return $ Floating (fromIntegral a*b)
			_                        -> liftThrows $ throwError (BadArg "(times num num)")
		divides l = case l of
			[Integral a, Integral b] -> return $ Integral (quot a b)
			[Floating a, Floating b] -> return $ Floating (a/b)
			[Floating a, Integral b] -> return $ Floating (a/fromIntegral b)
			[Integral a, Floating b] -> return $ Floating (fromIntegral a/b)
			_                        -> liftThrows $ throwError (BadArg "(divides num num)")
		equals l = case l of
			[a, b] -> return $ Boolean $ a == b
			_      -> liftThrows $ throwError (BadArg "(eq bool bool)")
		not_equals l = case l of
			[a, b] -> return $ Boolean $ a /= b
			_      -> liftThrows $ throwError (BadArg "(neq bool bool)")
		lower_than l = case l of
			[Integral a, Integral b] -> return $ Boolean $ a < b
			[Floating a, Floating b] -> return $ Boolean $ a < b
			[Symbol a, Symbol b]     -> return $ Boolean $ a < b
			[List a, List b]         -> return $ Boolean $ a < b
			[Quote a, Quote b]       -> return $ Boolean $ a < b
			_                        -> liftThrows $ throwError (BadArg "(lower_than ord ord)")
		greater_than l = case l of
			[Integral a, Integral b] -> return $ Boolean $ a > b
			[Floating a, Floating b] -> return $ Boolean $ a > b
			[Symbol a, Symbol b]     -> return $ Boolean $ a > b
			[List a, List b]         -> return $ Boolean $ a > b
			[Quote a, Quote b]       -> return $ Boolean $ a > b
			_                        -> liftThrows $ throwError (BadArg "(greater_than ord ord)")
		cons l = case l of
			[a, List b] -> return $ List $ a : b
			[a, b]      -> return $ List [a, b]
			_           -> liftThrows $ throwError (BadArg "(cons atom list|atom)")
		atom l = case l of
			[List []] -> return $ Boolean True
			[List _]  -> return $ Boolean False
			_         -> return $ Boolean True
		car l = case l of
			[List (a:_)] -> return a
			_            -> liftThrows $ throwError (BadArg "(car list)")
		cdr l = case l of
			[List (_:as)] -> return $  List as
			_             -> liftThrows $ throwError (BadArg "(cdr list)")

stdIOPrims :: [(Name, LispObject)]
stdIOPrims = map (\(n, f) -> (n, IOPrimitive n f)) [
			(":read", read_file),
			(":write", write_file),
			(":load", load)
		]
	where
		read_file l _ = case l of
			[String path] -> String <$> liftIO (readFile path)
			_             -> liftThrows $ throwError (BadArg "Expected argument of type String")
		write_file l _ = case l of
			[String path, o] -> liftIO (writeFile path (show o)) >> return (String "success")		
			_                -> liftThrows $ throwError (BadArg "Expected argument of type String")
		load l e = case l of 
			[String path] -> do
				contents <- get_string <$> read_file [String path] e
				case contents of
					Just s  -> (evalExpr . buildExpr $ run lispObject s) e >> return (String "success")
					Nothing -> throwError $ Default "Contents is not a string"
			_             -> throwError $ BadArg "Expected argument of type String"
		get_string (String s) = Just s
		get_string _          = Nothing

basis :: IO LispEnvironment
basis = emptyEnv >>= flip bindVars (stdPrims ++ stdIOPrims)

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
	fun <- evalExpr fn e
	as <- mapM (flip evalExpr e) args
	case fun of
		Primitive _ f   -> f as
		IOPrimitive _ f -> f as e
		Closure c b env -> (liftIO . bindVars env $ zip c as) >>= evalExpr b
		_               -> throwError $ BadExpr "(apply func args)"

evalExpr (ApplyOne fn args) e = do
	fun <- evalExpr fn e
	as <- evalExpr args e
	case (fun, as) of
		(Primitive _ f, List ass)   -> f ass
		(Primitive _ f, o)          -> f [o]
		(IOPrimitive _ f, List ass) -> f ass e
		(IOPrimitive _ f, o)        -> f [o] e
		(Closure c b env, List ass) -> (liftIO . bindVars env $ zip c ass) >>= evalExpr b
		(Closure c b env, o)        -> (liftIO . bindVars env $ zip c [o]) >>= evalExpr b
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
