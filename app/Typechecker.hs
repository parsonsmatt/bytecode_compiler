{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Typechecker (
    TypeError(..),
    TypecheckErrorType(..),
    TypeErrorSrc (..),
    TypeErrorContextSrc (..),
    TypeErrorContextType (..),
    typecheck''
) where

import qualified Data.Text as Text
import Ast
import Control.Monad
import Control.Applicative
-- import Control.Monad.Trans.State
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M
import Parser
import Control.Monad.IO.Class
import Data.Maybe
import Data.Either
import Debug.Trace
import Data.Functor
import Text.Megaparsec hiding (State)
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Data.List
import Data.Tuple.Extra
import CompilerError

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Right c) = Right c
mapLeft f (Left a) = Left (f a)

data TypecheckErrorType = VarNotDefined String
    | ReassignDiffType String Type Type
    | WrongType Type Type
    | NoReturnStmt
    | InvalidReturnType Type Type
    | UnexpectedReturnStmt
    | CallNonFunc Type
    | InvalidArgType
    | OtherErr String
    | CannotDeduceType String
    | AccessMutableFromInner String
    | ReassignImmutableVar String
    | InvalidOperatorType String Type Type
    | IfBranchesDifferent Type Type
    | VarUsedDiffTypes String
    | DuplicateLambdaParameter String
    | DuplicateVarAssign String
    deriving (Eq, Ord, Show)

tMsgString :: TypecheckErrorType -> String
tMsgString t = case t of
    VarNotDefined s -> "The variable you've used hasn't been defined anywhere." -- '" ++ s ++ "' is not defined"
    ReassignDiffType s t1 t2 -> "Variable '" ++ s ++ "' has a type of `" ++ show t1 ++ "`. You have attempted to reassign it to a type of `" ++ show t2 ++ "`."
    WrongType t1 t2 -> "A type of '" ++ show t1 ++ "' was expected, but you put in a type of '" ++ show t2 ++ "'"
    NoReturnStmt -> ""
    InvalidReturnType t1 t2 -> "Incorrect Return Type for Function"
    UnexpectedReturnStmt -> "Return Statement not nested in function."
    CallNonFunc t -> "Type '" ++ show t ++ "' is not a callable type."
    InvalidArgType -> ""
    OtherErr s -> s
    CannotDeduceType s -> "cannot deduce type of variable '" ++ s ++ "'"
    AccessMutableFromInner s -> "attempt to access mutable variable '" ++ s ++ "' from inner scope"
    ReassignImmutableVar s -> "Attempted reassign of immutable variable '" ++ s ++ "'."
    InvalidOperatorType s t1 t2 -> "The operator '" ++ s ++ "' expects a type '" ++ show t1 ++ "' but what you have put in has a type of '" ++ show t2 ++ "'"
    IfBranchesDifferent t1 t2 -> "The types for each if expression are different. The first branch has a type of '" ++ show t1 ++ "' and the second branch has a type of '" ++ show t2 ++ "'"
    VarUsedDiffTypes s -> "Cannot infer type of variable '" ++ s ++ "'. Used in different contexts."
    DuplicateLambdaParameter s -> "Duplicate lambda parameter '" ++ s ++ "'"
    DuplicateVarAssign s -> "Attempted redeclaration of variable '" ++ s ++ "'."

tMsgNotes :: TypecheckErrorType -> [String]
tMsgNotes t = case t of
    ReassignImmutableVar s -> ["Perhaps you could make '" ++ s ++ "' mutable?"]
    VarNotDefined s -> ["Perhaps it was misspelled?"]
    DuplicateLambdaParameter s -> ["You could rename the parameter I guess?"]
    DuplicateVarAssign s -> ["Remember that shadowing is not allowed in the same scope."]
    _ -> []

-- tMsgCode :: TypecheckErrorType -> String
-- tMsgCode t = case t of

tPosMsg :: TypecheckErrorType -> String
tPosMsg t = case t of
    VarNotDefined s -> "variable '" ++ s ++ "' is not defined"
    ReassignDiffType s t1 t2 -> "reassign to type `" ++ show t2 ++ "`"
    WrongType t1 t2 -> "wrong type of '" ++ show t2 ++ "'"
    NoReturnStmt -> "no return"
    InvalidReturnType t1 t2 -> "Expression has type of '" ++ show t2 ++ "' but '" ++ show t1 ++ "' was expected."
    UnexpectedReturnStmt -> "Here you have written a return statement, but it is not contained within a function 😕."
    CallNonFunc t -> "called here"
    InvalidArgType -> ""
    OtherErr s -> s
    CannotDeduceType s -> "cannot deduce type of variable '" ++ s ++ "'"
    AccessMutableFromInner s -> "attempt to access mutable variable '" ++ s ++ "' from inner scope"
    ReassignImmutableVar s -> "attempted reassign of '" ++ s ++ "'"
    InvalidOperatorType s t1 t2 -> "has the type '" ++ show t2 ++ "'"
    IfBranchesDifferent t1 t2 -> "has the type '" ++ show t2 ++ "'"
    VarUsedDiffTypes s -> "different types inferred for variable '" ++ s ++ "'"
    DuplicateLambdaParameter s -> "defined here"
    DuplicateVarAssign s -> "variable declaration"

data TypeErrorContextType = DeclaredHere
    | DeclaredWithType Type
    | EarlierBranch Type
    | InferredHereWithType Type
    | DefinedEarlierHere
    | DeclaredEarlierHere
    deriving (Eq, Ord, Show)

ctxPosMsg :: TypeErrorContextType -> String
ctxPosMsg ctx = case ctx of
    DeclaredHere -> "declared here"
    DeclaredWithType t -> "declared with type `" ++ show t ++ "`"
    EarlierBranch t -> "branch has type '" ++ show t ++ "'"
    InferredHereWithType t -> case t of
        NOTYPE -> "type couldn't be inferred"
        t -> "infered here as type '" ++ show t ++ "'"
    DefinedEarlierHere -> "defined earlier here"
    DeclaredEarlierHere -> "declared earlier here"

data TypeErrorContext = TypeErrorContext  {
    ctxType :: TypeErrorContextType,
    ctxOffset :: Position
} deriving (Eq, Ord, Show)

data TypeError = TypeError {
    tErrType :: TypecheckErrorType,
    offset :: Position,
    context :: [TypeErrorContext]
} deriving (Eq, Ord, Show)

data TypeErrorSrc = TypeErrorSrc {
    _tErrType :: TypecheckErrorType,
    _src :: (SourcePos, SourcePos),
    _ctx :: [TypeErrorContextSrc]
} deriving Show

instance CompileResult TypeErrorSrc where
    msgCode a = Nothing
    msgString a = tMsgString (_tErrType a)
    resultType _ = CompileError
    resultMsg a = CompileResultMsg (_src a) (tPosMsg $ _tErrType a) (tMsgNotes $ _tErrType a)
    ctxMsgs a = map (\ctx -> ResultContextMsg (_ctxSrc ctx) (ctxPosMsg $ _ctxType ctx)) (_ctx a)

data TypeErrorContextSrc = TypeErrorContextSrc {
    _ctxType :: TypeErrorContextType,
    _ctxSrc :: (SourcePos, SourcePos)
} deriving Show

type Span = (SourcePos, SourcePos)

-- data TypeError' = TypeError' TypecheckErrorType Span

contextConvert :: M.Map Position (SourcePos, SourcePos) -> TypeErrorContext -> TypeErrorContextSrc
contextConvert m ctx = TypeErrorContextSrc {_ctxType = ctxType ctx, _ctxSrc = m M.! ctxOffset ctx}

errConvert :: M.Map Position (SourcePos, SourcePos) -> TypeError -> TypeErrorSrc
errConvert m t = TypeErrorSrc {
    _tErrType = tErrType t,
    _src = m M.! offset t,
    _ctx = map (contextConvert m) (context t)
}

collectPositions :: [TypeError] -> [Position]
collectPositions = concatMap (\t -> offset t : map ctxOffset (context t))

instance ShowErrorComponent TypeError where
    showErrorComponent a = case tErrType a of
        VarNotDefined s -> "variable `" ++ s ++ "` hasn't been defined"
        WrongType x y -> "expected type `" ++ show x ++ "`, got `" ++ show y ++ "`"
        CannotDeduceType s -> "cannot deduce type for variable \"" ++ s ++ "\""
        InvalidReturnType x y -> "function expected return type of \"" ++ show x ++ "\" but got type \"" ++ show y ++ "\""
        AccessMutableFromInner x -> "cannot access outer mutable variable \"" ++ x ++ "\" from inner scope"
        ReassignImmutableVar x -> "cannot reassign immutable variable \"" ++ x ++ "\""
        s -> show s
    errorComponentLen a = posLength $ offset a

makePosState :: String -> a -> PosState a
makePosState filename s = PosState
    { pstateInput = s
    , pstateOffset = 0
    , pstateSourcePos = initialPos filename
    , pstateTabWidth = defaultTabWidth
    , pstateLinePrefix = ""
    }

toParseError :: [TypeError] -> PosState String -> ParseErrorBundle String TypeError
toParseError errs state = ParseErrorBundle
    { --bundleErrors = NE.singleton (FancyError (posOffset $ offset err) (S.singleton (ErrorCustom err)))
        bundleErrors = NE.map (\err -> FancyError (posOffset $ offset err) (S.singleton (ErrorCustom err))) (NE.fromList errs)
    , bundlePosState = state
    }

isFunction :: Type -> Bool
isFunction (FUNCTION _ _) = True
isFunction _ = False

type Mutable = Bool

data Env = Env {
    prevEnv :: Maybe Env,
    variables :: M.Map String (Maybe Type, Mutable, Position),
    lambdaParams :: M.Map String Position,
    unknowns :: S.Set (String, Type, Position),
    returnType :: Maybe Type,
    includesReturn :: Bool,
    typeErrors :: [TypeError]
    -- typeErrors'' :: [TypecheckNode]
}

newScope :: TypeChecker ()
newScope = modify (\env -> newEnv {prevEnv=Just env})

prevScope :: TypeChecker ()
prevScope = modify (fromJust . prevEnv)

setReturnType :: Type -> TypeChecker ()
setReturnType t = modify (\env -> env {returnType = Just t})

getReturnType :: TypeChecker (Maybe Type)
getReturnType = returnType <$> get

newEnv :: Env
newEnv = Env Nothing M.empty M.empty S.empty Nothing False []

varAssign :: String -> Maybe Type -> Position -> TypeChecker ()
varAssign s t pos = do
    vars <- variables <$> get
    case M.lookup s vars of
        Nothing -> modify (\env -> env {variables=M.insert s (t, True, pos) vars})
        Just x -> do
            addError $ TypeError (DuplicateVarAssign s) pos [TypeErrorContext DeclaredEarlierHere (thd3 x)]

letAssign :: String -> Maybe Type -> Position -> TypeChecker ()
letAssign s t pos = do
    vars <- variables <$> get
    case M.lookup s vars of
        Nothing -> modify (\env -> env {variables=M.insert s (t, False, pos) vars})
        Just x -> addError $ TypeError (DuplicateVarAssign s) pos [TypeErrorContext DeclaredEarlierHere (thd3 x)]

addVariable :: String -> Maybe Type -> Position -> TypeChecker ()
addVariable s t pos = do
    vars <- variables <$> get
    modify (\env -> env {variables=M.insert s (t, True, pos) vars})

addLet :: String -> Type -> Position -> TypeChecker ()
addLet s t pos = do
    vars <- variables <$> get
    modify (\env -> env {variables=M.insert s (Just t, False, pos) vars})

addUnknown :: String -> Type -> Position -> TypeChecker ()
addUnknown s t pos = do
    unknowns <- unknowns <$> get
    modify (\env -> env {unknowns = S.insert (s, t, pos) unknowns})

addLambdaParam :: (String, Position) -> TypeChecker ()
addLambdaParam (s, pos) = do
    lambdaParams <- lambdaParams <$> get
    case M.lookup s lambdaParams of
        Just pos' -> addError $ TypeError (DuplicateLambdaParameter s) pos [TypeErrorContext DefinedEarlierHere pos']
        Nothing -> return ()
    modify (\env -> env {lambdaParams = M.insert s pos lambdaParams})

isUnknown :: String -> TypeChecker Bool
isUnknown s = gets (M.member s . lambdaParams)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a m = case m of
    Just b -> Right b
    Nothing -> Left a

getStored :: String -> Position -> TypeChecker (Maybe (Maybe Type, Mutable, Position))
getStored s pos = do
    r <- (M.!? s) . variables <$> get
    case r of
        Just r -> return (Just r)
        Nothing -> addError (TypeError (VarNotDefined s) pos []) >> return Nothing

getVar :: String -> Position -> TypeChecker (Maybe Type)
getVar s pos = do
    store <- getStored s pos
    return $ store >>= \store -> if snd3 store then fst3 store else Nothing

getLet :: String -> Position -> TypeChecker (Maybe Type)
getLet s pos = do
    store <- getStored s pos
    return $ store >>= \store -> if not (snd3 store) then fst3 store else Nothing

checkVariable :: String -> Position -> Type -> TypeChecker (Maybe Type)
checkVariable s pos t = do
    lambdaParams <- gets lambdaParams
    if M.member s lambdaParams then do
        addUnknown s t pos
        return $ Just t
    else getVariable s pos

inferVariable :: String -> Position -> TypeChecker (Maybe Type)
inferVariable s pos = do
    lambdaParams <- gets lambdaParams
    if M.member s lambdaParams then do
        addUnknown s NOTYPE pos
        return Nothing
    else getVariable s pos

getVariable :: String -> Position -> TypeChecker (Maybe Type)
getVariable s pos = do
    env <- get
    t <- get' env
    r <- inScope s
    case t of
        Just (t, m, pos') -> do
            when (not r && m) $ do
                (addError (TypeError (AccessMutableFromInner s) pos [TypeErrorContext DeclaredHere pos']))
            return t
        Nothing -> addError (TypeError (VarNotDefined s) pos []) >> return Nothing
      where
        get' :: Env -> TypeChecker (Maybe (Maybe Type, Mutable, Position))
        get' env = case prevEnv env of
            Nothing -> return $ (\(t, m, p) -> (t, m, p)) <$> (M.!? s) (variables env)
            Just env' -> case (M.!? s) (variables env) of
                Just (t, m, p) -> return (Just (t, m, p))
                Nothing -> get' env'

getLambdaParam :: (String, Position) -> TypeChecker Type
getLambdaParam (s, pos) = do
    r <- get >>= get'
    case r of
        Just r -> return r
        Nothing -> addError (TypeError (CannotDeduceType s) pos []) >> return INT
  where
    get' :: Env -> TypeChecker (Maybe Type)
    get' env = case prevEnv env of
        Nothing -> return $ fst3 =<< (M.!? s) (variables env)
        Just env' -> case (M.!? s) (variables env) of
            Just t -> return (fst3 t)
            Nothing -> get' env'

inScope :: String -> TypeChecker Bool
inScope s = gets (M.member s . variables)

getVariableMaybe :: String -> Position -> TypeChecker (Maybe Type)
getVariableMaybe s pos = do
    env <- get
    t <- get' env
    r <- inScope s
    when (not r && (snd <$> t) == Just True) (addError (TypeError (AccessMutableFromInner s) pos []))
    return (fst =<< t)
  where
    get' :: Env -> TypeChecker (Maybe (Maybe Type, Mutable))
    get' env = case prevEnv env of
        Nothing -> return $ (\(t, m, _) -> (t, m)) <$> (M.!? s) (variables env)
        Just env' -> case (M.!? s) (variables env) of
            Just (t, m, _) -> return (Just (t, m))
            Nothing -> get' env'

-- getVariableMaybe :: String -> TypeChecker (Maybe Type)
-- getVariableMaybe s = get >>= get'
--   where
--     get' :: Env -> TypeChecker (Maybe Type)
--     get' env = case prevEnv env of
--         Nothing -> return $ fst <$> (M.!? s) (variables env)
--         Just env' -> case (M.!? s) (variables env) of
--             Just t -> return (Just $ fst t)
--             Nothing -> get' env'

type TypeChecker a = State Env a

addError :: TypeError -> TypeChecker ()
addError err = modify (addErr err)
  where
    addErr :: TypeError -> Env -> Env
    addErr err env = case prevEnv env of
        Nothing -> env {typeErrors = err : typeErrors env}
        Just env' -> env {prevEnv = (Just $ addErr err env')}

-- typecheck :: AST -> String -> Either (ParseErrorBundle String TypeError) TAST
-- typecheck ast text
--     | (not . null) (typeErrors env) = do
--         Left $ toParseError (sortBy (\x y -> compare x.offset y.offset) $ typeErrors env) (makePosState "" text)
--     | otherwise = Right s
--   where (s, env) = runState (typecheckAst ast) newEnv

annotatePositions :: (TraversableStream s) => s -> String -> [Position] -> M.Map Position (SourcePos, SourcePos)
annotatePositions s filename positions =
    let positions' = zip positions [0..]
        both' = concatMap (\(pos, n) -> [(n, pos.posOffset), (n, pos.posOffset + pos.posLength)]) positions'
        sorted = sortBy (\x y -> compare (snd x) (snd y)) both'
        annotate = sortBy (\x y -> compare (fst x) (fst y)) $ map (\((n, _), pos) -> (n, pos)) $ fst $ attachSourcePos snd sorted (makePosState filename s)
    in M.fromList $ zipWith (\x y -> (x, snd y)) positions (groupTwo annotate)
  where
    groupTwo :: [(Int, SourcePos)] -> [(Int, (SourcePos, SourcePos))]
    groupTwo [] = []
    groupTwo xs =
        let ([x, y], rest) = splitAt 2 xs
        in (fst x, (snd x, snd y)) : groupTwo rest

typecheck'' :: AST -> String -> String -> Either [TypeErrorSrc] TAST
typecheck'' ast text filename
    | (not . null) (typeErrors env) = do
        let errs = typeErrors env
        let positions = collectPositions errs
        let annotated = annotatePositions text filename positions
        let converted = map (errConvert annotated) errs
        Left converted
    | otherwise = Right s
  where (s, env) = runState (typecheckAst ast) newEnv

-- typecheck :: AST -> String -> Either (ParseErrorBundle String TypeError) TAST
-- typecheck ast text
--     | (not . null) (typeErrors env) = do
--         let errs = typeErrors' env
--         let supportNodes' = supportNodes env
--         let support = map (\e -> (Just e, SNode $ supportNodes' M.! e)) (concatMap supporting errs)
--         let errs' = map (\e -> (Nothing :: Maybe Int, TNode e)) errs
--         traceM (show errs)
--         traceM (show support)
--         Left $ toParseError (sortBy (\x y -> compare x.offset y.offset) $ typeErrors env) (makePosState "" text)
--     | otherwise = Right s
--   where (s, env) = runState (typecheckAst ast) newEnv

-- typecheck :: AST -> String -> Either (ParseErrorBundle String TypeError) TAST
-- typecheck ast text
--     | (not . null) (typeErrors env) = do
--         Left $ toParseError (sortBy (\x y -> compare x.offset y.offset) $ typeErrors env) (makePosState "" text)
--     | otherwise = Right s
--   where (s, env) = runState (typecheckAst ast) newEnv

-- typecheck :: AST -> String -> Either String TAST
-- typecheck ast text
--     | (not . null) (typeErrors env) = do
--         Left $ errorBundlePretty $ toParseError (sortBy (\x y -> compare x.offset y.offset) $ typeErrors env) (makePosState "" text)
--     | otherwise = Right s
--   where (s, env) = runState (typecheckAst ast) newEnv

typecheckAst :: AST -> TypeChecker TAST
typecheckAst ast = do
    collectFuncDefs ast
    stmts <- typecheckStmts ast
    vars <- variables <$> get
    -- traceM (show vars)
    return (stmts)

collectFuncDefs :: [Stmt] -> TypeChecker ()
collectFuncDefs stmts = do
    forM_ stmts $ \stmt -> case stmt.stmtType of
        (FuncDef s args returnType stmts) -> letAssign s (Just $ FUNCTION (map snd args) returnType) (stmt.stmtPos)
        _ -> return ()

typecheckFuncStmts :: [Stmt] -> TypeChecker [TStmt]
typecheckFuncStmts stmts = do
    collectFuncDefs stmts
    stmts <- typecheckStmts stmts
    vars <- variables <$> get
    -- traceM (show vars)
    return (vars `seq` stmts)

typecheckStmts :: [Stmt] -> TypeChecker [TStmt]
typecheckStmts [] = return []
typecheckStmts (x:xs) = do
    y <- typecheckStmt' x
    ys <- typecheckStmts xs
    return $ maybe id (:) y ys

reassignVar :: String -> Exp -> Position -> TypeChecker (Maybe TStmtType)
reassignVar s exp pos = do
    store <- getStored s pos
    case store of
        Nothing -> error $ "not found: " ++ s
        Just (Nothing, _, _) -> error "kjlgasdg"
        Just (Just t, m, decP) -> do
            unless m $ do
                addError (TypeError (ReassignImmutableVar s) pos [TypeErrorContext DeclaredHere decP])
            inferExp exp >>= \case
                Nothing -> error ""
                Just x -> do
                    when (m && x.tExpType /= t) $ do
                        addError (TypeError (ReassignDiffType s t x.tExpType) pos [TypeErrorContext (DeclaredWithType t) decP])
                    return (Just (TVarReassign s x))

typecheckStmt' :: Stmt -> TypeChecker (Maybe TStmt)
typecheckStmt' stmt = (\v -> v >>= \v -> return $ TStmt v stmt.stmtPos) <$> case stmt.stmtType of
    VarAssign s exp -> do
        x <- inferExp exp
        varAssign s (tExpType <$> x) stmt.stmtPos
        return (TVarAssign s <$> x)
    LetAssign s exp -> do
        x <- inferExp exp
        letAssign s (tExpType <$> x) stmt.stmtPos
        return (TLetAssign s <$> x)
    VarReassign s exp -> do
        reassignVar s exp stmt.stmtPos
    Print exp -> do
        x <- inferExp exp
        return (TPrint <$> x)
    CallExp exp -> do
        x <- inferExp exp
        return (TCallExp <$> x)
    While exp stmts -> do
        x <- checkExp exp BOOL $ \exp -> do
            unless (exp.tExpType == BOOL) (addError $ TypeError (WrongType BOOL exp.tExpType) exp.tExpPos [])
        stmts <- typecheckStmts stmts
        return $ Just (TWhile x stmts)
    FuncDef s args returnType cases -> do
        addLet s (FUNCTION (map snd args) returnType) stmt.stmtPos
        cases' <- forM cases $ \case' -> do
            x <- typecheckFuncCase case' args returnType
            case x of
                Nothing -> error "typecheck error"
                Just x -> return x
        return $ Just (TFuncDef s args returnType cases')
      where
        typecheckFuncCase :: FuncCase -> [(String, Type)] -> Type -> TypeChecker (Maybe TFuncCase)
        typecheckFuncCase (FuncCase xs stmts) args r = do
            mapped <- forM (zip xs args) $ \(x, arg) -> do
                when (fst x /= fst arg) $ error ""
                case snd x of
                    Nothing -> return (fst x, Nothing)
                    Just exp -> do
                        newScope
                        addVariable (fst x) (Just $ snd arg) posInit
                        tExp <- checkExp exp BOOL $ \exp -> do
                            when (exp.tExpType /= BOOL) $ error "wrong type, expected bool"
                        prevScope
                        return (fst x, Just tExp)
            newScope
            mapM_ (\(s, t) -> letAssign s (Just t) posInit) args
            setReturnType r
            tStmts <- typecheckFuncStmts stmts
            hasReturn <- includesReturn <$> get
            case returnType of
                VOID -> return ()
                _ -> do
                    unless hasReturn (addError (TypeError NoReturnStmt stmt.stmtPos []))
            prevScope
            return $ Just (TFuncCase mapped tStmts)
    ReturnStmt exp -> do
        t <- getReturnType
        case t of
            Just t -> do
                a <- checkExp exp t $ \a ->
                    when (t /= a.tExpType) $ addError (TypeError (InvalidReturnType t a.tExpType) a.tExpPos [])
                modify (\env -> env {includesReturn=True})
                return $ Just (TReturnStmt a)
            Nothing -> do
                addError (TypeError UnexpectedReturnStmt stmt.stmtPos [])
                return $ Just (TReturnStmt (tAnyExpWrapper exp))
    -- s -> error (show s)

inferBinaryIntOp :: Exp -> Exp -> (TExp -> TExp -> TExpType) -> String -> TypeChecker (Maybe (TExpType, Type))
inferBinaryIntOp x y t s = do
    a <- checkExp x INT $ \a ->
        unless (a.tExpType == INT) (addError $ TypeError (InvalidOperatorType s INT a.tExpType) a.tExpPos [])
    b <- checkExp y INT $ \b ->
        unless (b.tExpType == INT) (addError $ TypeError (InvalidOperatorType s INT b.tExpType) b.tExpPos [])
    return $ Just (t a b, INT)

inferBinaryCompOp :: Exp -> Exp -> (TExp -> TExp -> TExpType) -> String -> TypeChecker (Maybe (TExpType, Type))
inferBinaryCompOp x y t s = do
    a <- checkExp x INT $ \a ->
        unless (a.tExpType == INT) (addError $ TypeError (InvalidOperatorType s INT a.tExpType) a.tExpPos [])
    b <- checkExp y INT $ \b ->
        unless (b.tExpType == INT) (addError $ TypeError (InvalidOperatorType s INT b.tExpType) b.tExpPos [])
    return $ Just (t a b, BOOL)

inferExp :: Exp -> TypeChecker (Maybe TExp)
inferExp exp = (>>= \(v, t') -> return $ TExp v exp.expPos t') <$> case exp.expType of
    Val v -> do
        let t = valueToType v
        return $ Just (TVal v, t)
    Var s -> do
        t <- inferVariable s exp.expPos
        return (t <&> \t -> (TVar s, t))
    Add x y -> inferBinaryIntOp x y TAdd "+"
    Sub x y -> inferBinaryIntOp x y TSub "-"
    Mul x y -> inferBinaryIntOp x y TMul "*"
    Div x y -> inferBinaryIntOp x y TDiv "/"
    Greater x y -> inferBinaryCompOp x y TGreater ">"
    GreaterEqual x y -> inferBinaryCompOp x y TGreaterEqual ">="
    Less x y -> inferBinaryCompOp x y TLess "<"
    LessEqual x y -> inferBinaryCompOp x y TLessEqual "<="
    If cond x y -> do
        cond' <- checkExp cond BOOL $ \cond' ->
            unless (cond'.tExpType == BOOL) (addError $ TypeError (WrongType BOOL cond'.tExpType) cond'.tExpPos [])
        a <- inferExp x
        b <- inferExp y
        let ifType = tExpType <$> (a <|> b)
        case (a, b) of
            (Just a, Just b) ->
                when (a.tExpType /= b.tExpType) (addError $ TypeError (IfBranchesDifferent a.tExpType b.tExpType) b.tExpPos [TypeErrorContext (EarlierBranch a.tExpType) a.tExpPos])
            _ -> return ()
        return $ ifType <&> \t -> (TIf cond'
            (fromMaybe (tAnyExpWrapper x) a)
            (fromMaybe (tAnyExpWrapper x) b), t)
    Lambda params exp -> do
        newScope
        forM_ params addLambdaParam
        exp' <- inferExp exp
        lambdaParams <- gets lambdaParams
        unknowns <- gets unknowns
        results <- unify
        prevScope
        case (exp', results) of
            (Just exp, Just results) -> do
                return $ Just (TLambda params exp, FUNCTION (map ((results M.!) . fst) params) exp.tExpType)
            _ -> return Nothing
    StaticType exp -> do
        x <- inferExp exp
        return (x <&> \x -> (TVal (String (Text.pack $ show x.tExpType)), STRING))
    CallFunc exp exps -> do
        x <- inferExp exp
        case x of
            Nothing -> return Nothing
            Just x -> case x.tExpType of
                FUNCTION params r -> do
                    exps' <- forM (zip exps params) $ \(exp, param) -> do
                        checkExp exp param $ \exp ->
                            unless (exp.tExpType == param) (addError $ TypeError (WrongType param exp.tExpType) exp.tExpPos [])
                    return $ Just (TCallFunc x exps', r)
                t -> do
                    forM_ exps inferExp
                    addError (TypeError (CallNonFunc t) x.tExpPos [])
                    return Nothing

-- | Wrap an Exp in an dummy TExp. Use this when you need a TExp but the type of the Exp cannot be infered.
tAnyExpWrapper :: Exp -> TExp
tAnyExpWrapper exp = TExp TAnyExp exp.expPos VOID

unify :: TypeChecker (Maybe (M.Map String Type))
unify = do
    lambdaParams <- gets lambdaParams
    unknowns <- gets unknowns
    check M.empty lambdaParams (S.toList unknowns)
  where
    check :: M.Map String (S.Set (Type, Position)) -> M.Map String Position -> [(String, Type, Position)] -> TypeChecker (Maybe (M.Map String Type))
    check m unknowns [] = do
        reportUnusedVarsErrors
        (M.fromList <$>) . sequence <$> reportMultipleTypesErrors
      where
        reportUnusedVarsErrors = do
            let new = M.filterWithKey (\k v -> not (k `M.member` m)) unknowns
            unless (M.null new) $ forM_ (M.toList new) $ \(s, pos) -> addError (TypeError (OtherErr $ "The type of variable '" ++ s ++ "' could not be inferrred.") pos [])
        reportMultipleTypesErrors = do
            forM (M.toList m) $ \(s, xs) ->
                let list = S.toList xs
                    types' = map fst list
                    types = S.fromList types'
                in case S.size types of
                0 -> error $ "something went wrong. why is the size 0: " ++ show s
                x | x > 1 -> do
                    let pos = unknowns M.! s
                    addError (TypeError (OtherErr $ "The type of variable '" ++ s ++ "' could not be inferrred.") pos [])
                    return Nothing
                _ -> return (Just (s, head types'))
    check m unknowns ((s, t, pos):xs) = check (M.insertWith S.union s (S.singleton (t, pos)) m) unknowns xs

checkExp :: Exp -> Type -> (TExp -> TypeChecker ()) -> TypeChecker TExp
checkExp exp t f = (\case
    Just (v, t') -> let e = TExp v exp.expPos t' in f e >> return e
    Nothing -> return $ tAnyExpWrapper exp) =<< case exp.expType of
    Val v -> do
        let t' = valueToType v
        return $ Just (TVal v, t')
    Var s -> do
        x <- checkVariable s exp.expPos t
        return $ case x of
            Just t -> Just (TVar s, t)
            Nothing -> Nothing
    If cond x y -> do
        cond' <- checkExp cond BOOL $ \cond' ->
            unless (cond'.tExpType == BOOL) (addError $ TypeError (WrongType BOOL cond'.tExpType) cond'.tExpPos [])
        a <- checkExp x t $ \a ->
            unless (a.tExpType == t) (addError $ TypeError (WrongType BOOL a.tExpType) a.tExpPos [])
        b <- checkExp y t $ \b ->
            unless (b.tExpType == t) (addError $ TypeError (WrongType BOOL b.tExpType) b.tExpPos [])
        return $ Just (TIf cond' a b, t)
    Add x y -> inferBinaryIntOp x y TAdd "+"
    Sub x y -> inferBinaryIntOp x y TSub "-"
    Mul x y -> inferBinaryIntOp x y TMul "*"
    Div x y -> inferBinaryIntOp x y TDiv "/"
    Greater x y -> inferBinaryCompOp x y TGreater ">"
    GreaterEqual x y -> inferBinaryCompOp x y TGreaterEqual ">="
    Less x y -> inferBinaryCompOp x y TLess "<"
    LessEqual x y -> inferBinaryCompOp x y TLessEqual "<="
    Lambda params exp' -> do
        case t of
            FUNCTION params' r -> do
                newScope
                when (length params /= length params') $ do
                    addError (TypeError (InvalidReturnType t t) exp.expPos [])
                forM_ (zip params params') $ \(p, p') -> do
                    addVariable (fst p) (Just p') (snd p)
                exp' <- checkExp exp' r $ \exp' -> do
                    when (exp'.tExpType /= r) $ do
                        addError (TypeError (InvalidReturnType t t) exp'.tExpPos [])
                prevScope
                return $ Just (TLambda params exp', t)
            _ -> addError (TypeError (OtherErr "lambda incorrect type") exp.expPos []) >> return Nothing
    CallFunc exp exps -> do
        x <- inferExp exp
        case x of
            Nothing -> return Nothing
            Just x -> case x.tExpType of
                FUNCTION params r -> do
                    exps' <- forM (zip exps params) $ \(exp, param) -> do
                        checkExp exp param $ \exp ->
                            unless (exp.tExpType == param) (addError $ TypeError (WrongType param exp.tExpType) exp.tExpPos [])
                    return $ Just (TCallFunc x exps', r)
                t -> do
                    forM_ exps inferExp
                    addError (TypeError (CallNonFunc t) x.tExpPos [])
                    return Nothing
    t -> error (show t)

typecheckStmt :: Stmt -> TypeChecker TStmt
typecheckStmt stmt = (\v -> TStmt v stmt.stmtPos) <$> case stmt.stmtType of
    VarAssign s exp -> do
        x <- typecheckExp' exp Nothing
        addVariable s (Just x.tExpType) stmt.stmtPos
        return (TVarAssign s x)
    LetAssign s exp -> do
        x <- typecheckExp' exp Nothing
        addLet s x.tExpType stmt.stmtPos
        return (TLetAssign s x)
    -- VarReassign s exp -> do
    --     reassignVar s exp stmt.stmtPos
        -- var <- getVar s stmt.stmtPos
        -- var <- getVariable s stmt.stmtPos
        -- x <- typecheckExp' exp var
        -- return (TVarReassign s x)
    Print exp -> do
        x <- typecheckExp' exp Nothing
        return (TPrint x)
    -- StaticType exp -> do
    --     x <- typecheckExp' exp Nothing
    --     traceM (show x)
    --     return (TPrint x)
    -- FuncDef s args returnType stmts -> do
    --     newScope
    --     mapM_ (\(s, t) -> addVariable s (Just t) posInit) args
    --     setReturnType returnType
    --     tStmts <- typecheckFuncStmts stmts
    --     hasReturn <- includesReturn <$> get
    --     case returnType of
    --         VOID -> return ()
    --         _ -> do
    --             unless hasReturn (addError (TypeError NoReturnStmt stmt.stmtPos []))
    --     prevScope
    --     addVariable s (Just $ FUNCTION (map snd args) returnType) stmt.stmtPos
    --     return (TFuncDef s args returnType tStmts)
    ReturnStmt exp -> do
        t <- getReturnType
        case t of
            Just t -> do
                a <- checkExp exp t $ \a ->
                    when (t /= a.tExpType) $ addError (TypeError (InvalidReturnType t a.tExpType) a.tExpPos [])
                modify (\env -> env {includesReturn=True})
                return (TReturnStmt a)
            Nothing -> do
                addError (TypeError UnexpectedReturnStmt stmt.stmtPos [])
                return (TReturnStmt (tAnyExpWrapper exp))
        -- return (TReturnStmt a)
    CallExp exp -> do
        a <- typecheckExp' exp Nothing
        return (TCallExp a)

-- | Check that a type matches the expected type and throw an error otherwise
--
-- Example:
--
-- @
-- correctType INT (Just BOOL) (Position 3 5)
-- @

correctType :: Type -> Maybe Type -> Position -> TypeChecker Type
correctType t (Just t') pos = do
    when (t /= t') (addError $ TypeError (WrongType t' t) pos [])
    return t'
correctType t Nothing _ = return t

typecheckExp' :: Exp -> Maybe Type -> TypeChecker TExp
typecheckExp' exp t = (\(v, t') -> TExp v exp.expPos t') <$> case exp.expType of
    Val v -> do
        t' <- correctType (valueToType v) t exp.expPos
        return (TVal v, t')
    Var s -> do
        t' <- getVariableMaybe s exp.expPos
        case t' of
            Just t' -> correctType t' t exp.expPos <&> \t -> (TVar s, t)
            Nothing -> case t of
                Just t -> addVariable s (Just t) exp.expPos >> return (TVar s, t)
                Nothing -> getVariable s exp.expPos <&> \t -> (TVar s, fromJust t)
    Add x y -> do
        a <- typecheckExp' x (Just INT)
        b <- typecheckExp' y (Just INT)
        return (TAdd a b, INT)
    Sub x y -> do
        a <- typecheckExp' x (Just INT)
        b <- typecheckExp' y (Just INT)
        return (TSub a b, INT)
    Mul x y -> do
        a <- typecheckExp' x (Just INT)
        b <- typecheckExp' y (Just INT)
        return (TMul a b, INT)
    Lambda params exp -> do
        t' <- typecheckExp' exp Nothing
        pTypes <- mapM getLambdaParam params
        return (TLambda params t', FUNCTION pTypes t'.tExpType)
    CallFunc exp' args -> do
        f <- typecheckExp' exp' Nothing
        case f.tExpType of
            FUNCTION params r -> do
                as <- mapM (\(a, p) -> typecheckExp' a (Just p)) (zip args params)
                return (TCallFunc f as, r)
            _ -> addError (TypeError (CallNonFunc f.tExpType) exp.expPos []) >> return (TCallFunc f [], f.tExpType)
    Equal x y -> do
        a <- typecheckExp' x (Just INT)
        b <- typecheckExp' y (Just INT)
        when (a.tExpType /= b.tExpType) (addError $ TypeError (OtherErr "types must match for equal") exp.expPos [])
        return (TEqual a b, a.tExpType)
    And x y -> do
        a <- typecheckExp' x (Just BOOL)
        b <- typecheckExp' y (Just BOOL)
        return (TAnd a b, BOOL)
    If cond x y -> do
        cond' <- typecheckExp' cond (Just BOOL)
        a <- typecheckExp' x t
        b <- typecheckExp' y t
        when (a.tExpType /= b.tExpType) (addError $ TypeError (OtherErr "types must match for if") exp.expPos [])
        return (TIf cond' a b, a.tExpType)
    StaticType exp -> do
        x <- typecheckExp' exp Nothing
        return (TVal (String (Text.pack $ show x.tExpType)), STRING)
    x -> error $ "couldn't typecheck: " ++ show x

valueIsOfType :: Value -> Type -> Bool
valueIsOfType (Int _) INT = True
valueIsOfType (String _) STRING = True
valueIsOfType (Bool _) BOOL = True
valueIsOfType _ _ = False

valueToType :: Value -> Type
valueToType Int{} = INT
valueToType String{} = STRING
valueToType Bool{} = BOOL
