module TypeAnnotation where

import Ast
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Parser (forceParse)
import Control.Monad (void)

-- data ConcreteType = CInt deriving (Eq, Ord, Show)

data TypeVariable = TVariable String
    | Function [TypeVariable] TypeVariable
    | Concrete Type deriving (Eq, Ord, Show)

data Env = Env {
    vars :: M.Map String TypeVariable,
    typeVars :: S.Set (TypeVariable, TypeVariable)
} deriving Show

newEnv = Env M.empty S.empty

addVar :: (String, TypeVariable) -> Annotater ()
addVar (s, t) = modify (\env -> env {vars = M.insert s t (vars env)})

getVar :: String -> Annotater TypeVariable
getVar s = gets (\env -> vars env M.! s)

addPair :: (TypeVariable, TypeVariable) -> Annotater ()
addPair t = do
    typeVars <- gets typeVars
    modify (\env -> env {typeVars = S.insert t typeVars})

newTypeVariable :: Annotater TypeVariable
newTypeVariable = do
    typeVars <- gets typeVars
    let length = S.size typeVars
    let newT = TVariable $ "t" ++ show (length + 1)
    return newT

addConcrete :: Type -> Annotater TypeVariable
addConcrete t = do
    typeVars <- gets typeVars
    let length = S.size typeVars
    let newT = TVariable $ "t" ++ show (length + 1)
    let new = S.insert (newT, Concrete t) typeVars
    modify (\env -> env {typeVars = new})
    return newT

addFunction :: [TypeVariable] -> TypeVariable -> Annotater TypeVariable
addFunction t1 t2 = do
    typeVars <- gets typeVars
    let length = S.size typeVars
    let newT = TVariable $ "t" ++ show (length + 1)
    let new = S.insert (newT, Function t1 t2) typeVars
    modify (\env -> env {typeVars = new})
    return newT

type Annotater a = State Env a

valueToType :: Value -> Type
valueToType Int{} = INT
valueToType String{} = STRING
valueToType Bool{} = BOOL

annotateAst :: [Stmt] -> Annotater ()
annotateAst [] = return ()
annotateAst (x:xs) = do
    (case stmtType x of
        VarAssign s exp -> do
            t <- annotateExp exp
            addVar (s, t)
        CallExp exp -> void (annotateExp exp)
        _ -> error "error no stmt")
    annotateAst xs

annotateExp :: Exp -> Annotater TypeVariable
annotateExp exp = case expType exp of
    Add x y -> do
        a <- annotateExp x
        b <- annotateExp y
        addPair (a, Concrete INT)
        addPair (b, Concrete INT)
        addFunction [Concrete INT, Concrete INT] (Concrete INT)
    If cond x y -> do
        t <- annotateExp cond
        addPair (t, Concrete BOOL)
        a <- annotateExp x
        b <- annotateExp y
        t <- newTypeVariable
        addPair (a, t)
        addPair (b, t)
        return t
    Val v -> addConcrete (valueToType v)
    Var s -> getVar s

getTypeVars :: String -> S.Set (TypeVariable, TypeVariable)
getTypeVars s = typeVars $ execState (annotateAst $ forceParse s) newEnv

test''' = getTypeVars "var x = 5 x + 8"

test :: String -> IO ()
test s = mapM_ print (typeVars $ execState (annotateAst $ forceParse s) newEnv)

test' = test "4 + 5 + True"
test'' = test "if True {3} else {8}"
-- test'' = test "var x = 5 x + 9"