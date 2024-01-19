module Ast where

data Value = Int Int
    | String String
    | Bool Bool
    deriving (Eq, Ord, Show)

data Type = VOID
    | INT
    | DOUBLE
    | STRING
    | BOOL
    | FUNCTION [Type] Type
    | NOTYPE -- When type cannot be inferred
        deriving (Eq, Show, Ord)

-- function add(x: Int, y: Int) -> Int {return x + y}
-- FUNCTION [INT, INT] INT
-- FUNCTION INT (FUNCTION INT INT)

-- data Stmt' = Stmt' {
--     _stmtType :: StmtType,
--     _stmtPos :: Position
-- }

data Stmt = Stmt {
    stmtType :: StmtType,
    stmtPos :: Position
} deriving (Eq, Ord, Show)

data FuncCase = FuncCase [(String, Maybe Exp)] [Stmt] deriving (Eq, Ord, Show)
data TFuncCase = TFuncCase [(String, Maybe TExp)] [TStmt] deriving (Eq, Ord, Show)

data StmtType = VarAssign String Exp
    | LetAssign String Exp
    | VarReassign String Exp
    | Print Exp
    | CallExp Exp
    | While Exp [Stmt]
    | FuncDef String [(String, Type)] Type [FuncCase]
    | ReturnStmt Exp
    deriving (Eq, Ord, Show)


data TStmt = TStmt {
    tStmtType :: TStmtType,
    tStmtPos :: Position
} deriving (Eq, Ord, Show)

data TStmtType = TVarAssign String TExp
    | TLetAssign String TExp
    | TVarReassign String TExp
    | TPrint TExp
    | TCallExp TExp
    | TWhile TExp [TStmt]
    | TFuncDef String [(String, Type)] Type [TFuncCase]
    | TReturnStmt TExp
    deriving (Eq, Ord, Show)

type AST = [Stmt]
type TAST = [TStmt]

data Position = Position {
    posOffset :: Int,
    posLength :: Int
} deriving (Eq, Ord, Show)

posInit :: Position
posInit = Position 0 0

data TExp = TExp {
    tExp :: TExpType,
    tExpPos :: Position,
    tExpType :: Type
} deriving (Eq, Ord, Show)

data TExpType = TAdd TExp TExp
    | TSub TExp TExp
    | TMul TExp TExp
    | TDiv TExp TExp
    | TVal Value
    | TVar String
    | TEqual TExp TExp
    | TNotEqual TExp TExp
    | TGreater TExp TExp
    | TGreaterEqual TExp TExp
    | TLess TExp TExp
    | TLessEqual TExp TExp
    | TAnd TExp TExp
    | TOr TExp TExp
    | TCallFunc TExp [TExp]
    | TLambda [(String, Position)] TExp
    | TIf TExp TExp TExp
    | TAnyExp -- For type failures
    deriving (Eq, Ord, Show)

data Exp = Exp {
    expType :: ExpType,
    expPos :: Position
} deriving (Eq, Ord, Show)

data ExpType = Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Val Value
    | Var String
    | Equal Exp Exp
    | NotEqual Exp Exp
    | Greater Exp Exp
    | GreaterEqual Exp Exp
    | Less Exp Exp
    | LessEqual Exp Exp
    | And Exp Exp
    | Or Exp Exp
    | CallFunc Exp [Exp]
    | Lambda [(String, Position)] Exp
    | If Exp Exp Exp
    | StaticType Exp
    deriving (Eq, Ord, Show)