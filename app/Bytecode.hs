{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Bytecode (
    Chunk (..),
    Program (..),
    Function (..),
    BytecodeValue (..),
    compileProgram,
    disassembleProgram
) where

import Opcode
import Ast
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.Trans.Class
import Parser
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Sequence as S
import Data.Sequence ((|>))
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.List
import Data.Word
import Data.Bits ( Bits((.&.), shiftL, shiftR) )
import Data.Foldable
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe hiding (fromJust)

-- | An offset in the bytecode

type Offset = Int

-- | Used as placeholders for jump instructions
--
-- Analogous to Int

type Label = Int

data BytecodeValue = BInt !Int | BString !T.Text | BBool !Bool | Func !Function deriving (Show, Eq, Ord)
type Compiler a = StateT CompilerEnv (Either String) a

valueToBytecodeValue :: Value -> BytecodeValue
valueToBytecodeValue (Int x) = BInt x
valueToBytecodeValue (String s) = BString (T.pack s)
valueToBytecodeValue (Bool b) = BBool b

data LocalVariable = LocalVariable {
    depth :: Int,
    name :: String
} deriving (Show, Eq, Ord)

data Upvalue = Upvalue {
    upvalueIndex :: Int,
    isLocal :: Bool
} deriving (Show, Eq, Ord)

data Chunk = Chunk {
    code :: {-# UNPACK #-} U.Vector Word8,
    constantsPool :: {-# UNPACK #-} V.Vector BytecodeValue
} deriving (Show, Eq, Ord)

modifyChunkCode :: Chunk -> (U.Vector Word8 -> U.Vector Word8) -> Chunk
modifyChunkCode chunk f = chunk { code = f (code chunk) }

newChunk :: Chunk
newChunk = Chunk U.empty V.empty

data Function = Function {
    chunk :: {-# UNPACK #-} !Chunk,
    funcUpvalues :: M.Map Int BytecodeValue
} deriving (Show, Eq, Ord)

newFunction :: Function
newFunction = Function newChunk M.empty

emitByte :: Word8 -> Compiler ()
emitByte n = do
    func <- function <$> get
    let newChunk = (chunk func) { code = code (chunk func) `U.snoc` n }
    modify (\env -> env { function = (func { chunk = newChunk }) })

enumToEnum :: (Enum a, Enum b) => a -> b
enumToEnum = toEnum . fromEnum

emitWord16 :: Word16 -> Compiler ()
emitWord16 word = do
    let byte = enumToEnum $ word `shiftR` 8
    let byte' = enumToEnum $ word .&. 0xff
    emitByte byte
    emitByte byte'

-- | This function will emit an Opcode in the current chunk
--
-- Example:
--
-- @
-- emitOpcode ADD
-- @

emitOpcode :: Opcode -> Compiler ()
emitOpcode = emitByte . toEnum . fromEnum

addConstant :: BytecodeValue -> Compiler Word16
addConstant value = do
    func <- function <$> get
    let length = V.length $ constantsPool $ chunk func
    let newChunk = (chunk func) { constantsPool = constantsPool (chunk func) `V.snoc` value}
    modify (\env -> env { function = func { chunk = newChunk } })
    return $ toEnum length

data CompilerEnv = CompilerEnv {
    enclosing :: Maybe CompilerEnv,
    globalStrs :: M.Map String Int,
    locals :: [LocalVariable],
    upvalues :: M.Map Int Upvalue,
    function :: Function,
    scopeDepth :: Int,
    jmpLabels :: S.Seq Offset,
    declarations :: M.Map String Int
} deriving (Show, Eq, Ord)

addDeclaration :: String -> Compiler ()
addDeclaration s = do
    declarations <- declarations <$> get
    if s `M.member` declarations then
        lift $ Left $ "duplicate declaration named `" ++ s ++ "`"
    else do
        let size = M.size declarations
        modify (\env -> env {declarations=M.insert s size declarations})

getGlobalStrs :: Compiler (M.Map String Int)
getGlobalStrs = strs <$> get
  where
    strs :: CompilerEnv -> M.Map String Int
    strs env = case enclosing env of
        Nothing -> globalStrs env
        Just env -> strs env

getGlobalDeclarations :: Compiler (M.Map String Int)
getGlobalDeclarations = strs <$> get
  where
    strs :: CompilerEnv -> M.Map String Int
    strs env = case enclosing env of
        Nothing -> declarations env
        Just env -> strs env

initCompiler :: CompilerEnv
initCompiler = CompilerEnv Nothing M.empty [] M.empty newFunction 0 S.empty M.empty

-- | Gets a new label without adding one to the environment
--
-- Definition
--
-- @
-- getLabel :: Compiler Label
-- getLabel = S.length . jmpLabels <$> get
-- @

getLabel :: Compiler Label
getLabel = S.length . jmpLabels <$> get

opcodesLength :: Compiler Int
opcodesLength = U.length . code . chunk . function <$> get

getOffset :: Compiler Offset
getOffset = opcodesLength

data Program = Program {
    mainChunk :: Chunk
} deriving Show

fromJust :: Maybe a -> a
fromJust (Just a) = a

disassembleCode :: U.Vector Word8 -> [(Opcode, [Word8])]
disassembleCode code
    | U.null code = []
    | otherwise =
        let (byte, rest) = fromJust $ U.uncons code
            opcode = toEnum $ fromEnum byte
            (bytes, rest') = U.splitAt (opcodeLength opcode) rest
        in (opcode, U.toList bytes) : disassembleCode rest'

opcodeLength :: Opcode -> Int
opcodeLength JUMP = 2
opcodeLength JUMP_TRUE = 2
opcodeLength JUMP_FALSE = 2
opcodeLength LOAD_CONST = 2
opcodeLength STORE_NAME = 2
opcodeLength LOAD_NAME = 2
opcodeLength MAKE_CLOSURE = 2
opcodeLength LOAD_UPVALUE = 2
opcodeLength LOAD_FAST = 2
opcodeLength STORE_FAST = 2
opcodeLength CALL = 1
opcodeLength _ = 0

disassembleProgram :: Program -> String
disassembleProgram (Program pChunk) =
    let constants = constantsPool pChunk
    in concat $ disassembleChunk pChunk

disassembleChunk :: Chunk -> [String]
disassembleChunk (Chunk code constants) =
    let disassembly = disassembleCode code
        curr = concat $ map (\(opcode, bytes) -> show opcode ++ "\t" ++ show bytes ++ "\n") disassembly
        rest = concat $ zipWith (\i s -> "\n\n" : byteValDisassemble s) [0..] (V.toList constants)
    in curr : rest
  where
    byteValDisassemble :: BytecodeValue -> [String]
    byteValDisassemble (Func function) = disassembleChunk (chunk function)
    byteValDisassemble _ = []

compileFunction :: Compiler Function
compileFunction = do
    fixLabels
    function <$> get

executingStateT :: Monad m => s -> StateT s m a -> m s
executingStateT = flip execStateT

compileProgram :: [TStmt] -> Either String Program
compileProgram stmts =
    let result = executingStateT initCompiler $ do
            compileAst stmts
            fixLabels
    in Program . chunk . function <$> result

compileAst :: [TStmt] -> Compiler ()
compileAst xs = compileStmts xs >> emitOpcode EXIT

enterFunction :: Compiler ()
enterFunction = do
    env <- get
    let newCompilerEnv = initCompiler { enclosing = Just env, scopeDepth = scopeDepth env + 1 }
    put newCompilerEnv

exitFunction :: Compiler ()
exitFunction = modify (fromJust . enclosing)

goPreviousScope :: Compiler Bool
goPreviousScope = do
    prev <- enclosing <$> get
    case prev of
        Just env -> put env >> return True
        Nothing -> return False

addCompilerScope :: CompilerEnv -> Compiler ()
addCompilerScope env = do
    prev <- get
    put (env { enclosing = Just prev })

modifyCode :: (U.Vector Word8 -> U.Vector Word8) -> Compiler ()
modifyCode f = do
    func <- function <$> get
    let newFunc = func { chunk = modifyChunkCode (chunk func) f }
    modify (\env -> env { function = newFunc })

bytesToWord16 :: [Word8] -> Word16
bytesToWord16 = foldl (\acc x -> acc `shiftL` 8 + toEnum (fromEnum x)) 0

word16ToBytes :: Word16 -> [Word8]
word16ToBytes 0 = [0]
word16ToBytes word =
    let byte = toEnum $ fromEnum word `shiftR` 8
        byte' = toEnum $ fromEnum word .&. 0xff
    in [byte, byte']

assemble :: [(Opcode, [Word8])] -> S.Seq Word8
assemble xs = execState (do
    forM_ xs $ \(opcode, bytes) -> do
        modify (\s -> (s |> toEnum (fromEnum opcode)) S.>< S.fromList bytes)) S.empty

fixLabels :: Compiler ()
fixLabels = do
    labels <- jmpLabels <$> get
    code <- code . chunk . function <$> get
    let disassembled = disassembleCode code
    let labelsFixed = flip map disassembled $ \c@(opcode, bytes) -> case opcode of
            JUMP_FALSE -> (JUMP_FALSE, word16ToBytes $ toEnum $ labels `S.index` fromEnum (bytesToWord16 bytes))
            JUMP_TRUE -> (JUMP_TRUE, word16ToBytes $ toEnum $ labels `S.index` fromEnum (bytesToWord16 bytes))
            JUMP -> (JUMP, word16ToBytes $ toEnum $ labels `S.index` fromEnum (bytesToWord16 bytes))
            _ -> c
    let newCode = assemble labelsFixed
    let opcodes = U.fromList (toList newCode)
    modifyCode (const opcodes)

addFunctionDeclarations :: [TStmt] -> Compiler ()
addFunctionDeclarations stmts = forM_ stmts $ \stmt -> case stmt.tStmtType of
    TFuncDef s _ _ _ -> addDeclaration s
    TVarAssign s _ -> addDeclaration s
    TLetAssign s _ -> addDeclaration s
    _ -> return ()

compileStmts :: [TStmt] -> Compiler ()
compileStmts stmts = do
    addFunctionDeclarations stmts
    go stmts
  where
    go :: [TStmt] -> Compiler ()
    go [] = return ()
    go (x:xs) = do
        compileStmt x
        go xs

compileStmt :: TStmt -> Compiler ()
compileStmt stmt = case tStmtType stmt of
    (TPrint exp) -> do
        compileExp exp
        emitOpcode PRINT
    (TVarAssign s exp) -> do
        compileExp exp
        addVariableStore s
    (TLetAssign s exp) -> do
        compileExp exp
        addVariableStore s
    (TVarReassign s exp) -> do
        compileExp exp
        reassignVariableStore s
    (TCallExp exp) -> compileExp exp
    (TWhile exp stmts) -> do
        startLabel <- addLabelCurrOffset

        compileExp exp
        expFalseLabel <- addLabel 0
        emitOpcode JUMP_FALSE
        emitWord16 (toEnum expFalseLabel)

        compileStmts stmts
        emitOpcode JUMP
        emitWord16 (toEnum startLabel)

        offset <- getOffset
        updateLabel expFalseLabel offset
    (TFuncDef name params returnType cases) -> do
        enterFunction
        forM_ (map fst params) addLocal
        endLabel <- addLabel 0
        nextCondition <- addLabel 0
        forM_ cases $ \t@(TFuncCase xs stmts) -> do
            next <- addLabel 0
            compileCase t next endLabel
            offset <- getOffset
            updateLabel next offset
        compileStmt (TStmt (TPrint (TExp (TVal (String "error, no path for function")) posInit STRING)) posInit)
        compileStmt (TStmt (TReturnStmt (TExp (TVal (Int 0)) posInit INT)) posInit)
        offset <- getOffset
        updateLabel endLabel offset
        func <- compileFunction
        upvalues' <- upvalues <$> get
        exitFunction
        i <- addConstant (Func func)
        emitOpcode LOAD_CONST
        emitWord16 i
        emitOpcode MAKE_CLOSURE
        emitWord16 (toEnum $ M.size upvalues')
        forM_ (M.toList upvalues') $ \(key, value) -> do
            emitByte (toEnum (upvalueIndex value))
            emitByte (toEnum (if isLocal value then 1 else 0))
        addVariableStore name
      where
        compileCase :: TFuncCase -> Label -> Label -> Compiler ()
        compileCase t@(TFuncCase xs stmts) next end = do
            let exps = catMaybes (map snd xs)
            forM_ exps $ \exp -> do
                compileExp exp
                emitOpcode JUMP_FALSE
                emitWord16 (toEnum next)
            compileStmts stmts
            compileStmt (TStmt (TReturnStmt (TExp (TVal (Int 0)) posInit INT)) posInit)
    (TReturnStmt exp) -> do
        compileExp exp
        emitOpcode RET

binOp :: Opcode -> TExp -> TExp -> Compiler ()
binOp opcode x y = do
    compileExp x
    compileExp y
    emitOpcode opcode

resolveLocalUpvalue :: String -> Compiler (Maybe Int)
resolveLocalUpvalue s = do
    localVars <- locals <$> get
    case find (\(_, local) -> s == name local) (zip [0..] (reverse localVars)) of
        Just (i, var) -> do
            return $ Just i
        Nothing -> return Nothing

addUpvalue :: Upvalue -> Compiler ()
addUpvalue value = do
    env <- get
    let length = M.size (upvalues env)
    let newUpvalues = M.insert length value (upvalues env)
    modify (\env -> env { upvalues = newUpvalues })

resolveUpvalue :: String -> Compiler ()
resolveUpvalue name = do
    depth <- scopeDepth <$> get
    when (depth == 0) (lift $ Left $ "Upvalue error: no variable named `" ++ name ++ "`")
    current <- get
    goPreviousScope
    local <- resolveLocalUpvalue name
    case local of
        Just i -> do
            addCompilerScope current
            addUpvalue (Upvalue i True)
            emitOpcode LOAD_UPVALUE
            emitWord16 (toEnum i)
        Nothing -> do
            resolveUpvalue name
            addCompilerScope current

addVariableGlobal :: String -> Compiler ()
addVariableGlobal s = do
    declarations <- getGlobalDeclarations
    case declarations M.!? s of
        Just i -> emitOpcode LOAD_NAME >> emitWord16 (toEnum i)
        Nothing -> lift $ Left $ "No variable named `" ++ s ++ "`"

addVariableLoadNonLocal :: String -> Compiler ()
addVariableLoadNonLocal s = do
    previous <- enclosing <$> get
    depth <- scopeDepth <$> get
    case depth of
        0 -> addVariableGlobal s
        1 -> addVariableGlobal s
        _ -> resolveUpvalue s

addVariableLoad :: String -> Compiler ()
addVariableLoad s = do
    variables <- locals <$> get
    let len = length variables
    case s `elemIndex` map name variables of
        Just index -> emitOpcode LOAD_FAST >> emitWord16 (toEnum (len - index - 1))
        Nothing -> addVariableLoadNonLocal s

addGlobalStore :: String -> Compiler ()
addGlobalStore s = do
    globalStrs <- globalStrs <$> get
    emitOpcode STORE_NAME
    case M.lookup s globalStrs of
        Just idx -> lift $ Left $ "Duplicate variable named `" ++ s ++ "`"
        Nothing -> do
            let globalSize = length globalStrs
            modify (\env -> env { globalStrs = M.insert s globalSize globalStrs })
            emitWord16 (toEnum globalSize)

addStoreForExisting :: String -> Compiler ()
addStoreForExisting s = do
    depth <- scopeDepth <$> get
    len <- length . locals <$> get
    case depth of
        0 -> do
            globalStrs <- globalStrs <$> get
            emitOpcode STORE_NAME
            case M.lookup s globalStrs of
                Just idx -> emitWord16 (toEnum idx)
                Nothing -> lift $ Left $ "Catastrophic error: variable `" ++ s ++ "` does not actually exist"
        _ -> do
            locals <- locals <$> get
            depth <- scopeDepth <$> get
            emitOpcode STORE_FAST
            case find (\(LocalVariable d s', i) -> s == s') (zip (reverse locals) [0..]) of
                Just (_, idx) -> emitWord16 (toEnum idx)
                Nothing -> lift $ Left $ "Catastrophic error: variable `" ++ s ++ "` does not actually exist"

reserveVariableStore :: String -> Compiler ()
reserveVariableStore s = do
    depth <- scopeDepth <$> get
    case depth of
        0 -> do
            globalStrs <- globalStrs <$> get
            case M.lookup s globalStrs of
                Just idx -> lift $ Left $ "Duplicate variable named `" ++ s ++ "`"
                Nothing -> do
                    let globalSize = length globalStrs
                    modify (\env -> env { globalStrs = M.insert s globalSize globalStrs })
        _ -> do
            locals <- locals <$> get
            let newLocal = LocalVariable depth s
            case find (== newLocal) locals of
                Just x -> lift $ Left $ "Duplicate local variable named `" ++ s ++ "`"
                Nothing -> modify (\env -> env { locals = newLocal : locals })

addVariableStore :: String -> Compiler ()
addVariableStore s = do
    depth <- scopeDepth <$> get
    len <- length . locals <$> get
    case depth of
        0 -> addGlobalStore s
        _ -> addLocal s >> emitOpcode STORE_FAST >> emitWord16 (toEnum len)

reassignVariableStore :: String -> Compiler ()
reassignVariableStore s = do
    depth <- scopeDepth <$> get
    case depth of
        0 -> global s
        _ -> local s
  where
    global :: String -> Compiler ()
    global s = do
        globalStrs <- globalStrs <$> get
        emitOpcode STORE_NAME
        case globalStrs M.!? s of
            Just v -> emitWord16 (toEnum v) >> modify (\env -> env {globalStrs = M.insert s v globalStrs})
            Nothing -> lift $ Left $ "variable named `" ++ s ++ "` doesn't exist"
    local :: String -> Compiler ()
    local s = do
        locals <- locals <$> get
        depth <- scopeDepth <$> get
        emitOpcode STORE_FAST
        case find (\(LocalVariable d s', n) -> d == depth && s' == s) (zip (reverse locals) [0..]) of
            Just (l@(LocalVariable d s'), n) -> do
                emitWord16 (toEnum n)
                let locals' = map (\l' -> if l == l' then l' else l) locals
                modify (\env -> env{locals=locals'})

addLocal :: String -> Compiler ()
addLocal s = do
    locals <- locals <$> get
    depth <- scopeDepth <$> get
    let newLocal = LocalVariable depth s
    case find (== newLocal) locals of
        Just x -> lift $ Left $ "Duplicate local variable named `" ++ s ++ "`"
        Nothing -> modify (\env -> env { locals = newLocal : locals })

-- | Add label that points to the current offset in the bytecode
addLabelCurrOffset :: Compiler Label
addLabelCurrOffset = do
    offset <- opcodesLength
    label <- getLabel
    modify (\env -> env { jmpLabels = jmpLabels env |> offset })
    return label

-- | Update label to point to new offset in the bytecode
updateLabel :: Label -> Offset -> Compiler ()
updateLabel label offset = do
    env <- get
    put (env { jmpLabels = S.update label offset (jmpLabels env) })

-- | Add a new label pointing to a specific offset in the bytecode
addLabel :: Offset -> Compiler Label
addLabel offset = do
    label <- getLabel
    modify (\env -> env { jmpLabels = jmpLabels env |> offset })
    return label

-- | Add a new label initialized to an offset of 0
--
-- Used for labels that will be updated with the correct offset later
addDummyLabel :: Compiler Label
addDummyLabel = addLabel 0

compileExp :: TExp -> Compiler ()
compileExp exp = case tExp exp of
    (TAdd x y) -> binOp ADD x y
    (TSub x y) -> binOp SUB x y
    (TMul x y) -> binOp MUL x y
    (TDiv x y) -> binOp DIV x y
    (TVal x) -> do
        i <- addConstant (valueToBytecodeValue x)
        emitOpcode LOAD_CONST
        emitWord16 i
    (TVar s) -> addVariableLoad s
    (TEqual x y) -> binOp EQUAL x y
    (TNotEqual x y) -> binOp NOT_EQUAL x y
    (TGreater x y) -> binOp GREATER x y
    (TGreaterEqual x y) -> binOp GREATER_EQUAL x y
    (TLess x y) -> binOp LESS x y
    (TLessEqual x y) -> binOp LESS_EQUAL x y
    (TAnd x y) -> do
        compileExp x
        label <- addLabel 0
        emitOpcode DUPE_LAST
        emitOpcode JUMP_FALSE
        emitWord16 (toEnum label)
        emitOpcode POP
        compileExp y
        offset <- opcodesLength
        updateLabel label offset
    (TOr x y) -> do
        compileExp x
        label <- getLabel
        emitOpcode DUPE_LAST
        emitOpcode JUMP_TRUE
        emitWord16 (toEnum label)
        emitOpcode POP
        compileExp y
        offset <- opcodesLength
        void $ addLabel offset
    (TIf cond if' else') -> do
        -- This label will point to the code after the if expression
        endLabel <- addLabel 0
        -- Evaluate if condition
        compileExp cond
        -- The Label pointing to the else branch
        elseLabel <- addLabel 0
        -- Jump to else branch if expression is false
        emitOpcode JUMP_FALSE
        emitWord16 (toEnum elseLabel)
        compileExp if'
        -- Unconditional jump after executing if branch
        emitOpcode JUMP
        emitWord16 (toEnum endLabel)
        -- Update offset for else branch
        offset <- opcodesLength
        updateLabel elseLabel offset
        compileExp else'
        -- Update offset for end of entire if expression
        offset <- opcodesLength
        updateLabel endLabel offset
    (TCallFunc exp args) -> do
        compileExp exp
        forM_ args compileExp
        emitOpcode CALL
        emitByte (toEnum $ length args)
    (TLambda params exp) -> do
        enterFunction
        forM_ (map fst params) addLocal
        compileExp exp
        emitOpcode RET
        func <- compileFunction
        upvalues' <- upvalues <$> get
        exitFunction
        i <- addConstant (Func func)
        emitOpcode LOAD_CONST
        emitWord16 i
        emitOpcode MAKE_CLOSURE
        emitWord16 (toEnum $ M.size upvalues')
        forM_ (M.toList upvalues') $ \(key, value) -> do
            emitByte (toEnum (upvalueIndex value))
            emitByte (toEnum (if isLocal value then 1 else 0))
    s -> error (show s)
