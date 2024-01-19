{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
module VM where

import Bytecode
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Either.Extra
import Control.Monad
import Ast (Value(..))
import Opcode
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Map.Strict as M
import Data.Word
import Data.Bits
import Data.Maybe
import Debug.Trace

type CONSTANTS_TABLE = [Value]

type LocalVariables = M.Map Int BytecodeValue

data CallFrame = CallFrame {
    function' :: {-# UNPACK #-} !Function,
    locals :: !LocalVariables,
    ip :: {-# UNPACK #-} !Int
} deriving Show

data Env = Env {
    prevCallFrames :: [CallFrame],
    currentCallFrame :: {-# UNPACK #-} !CallFrame,
    stack :: [BytecodeValue],
    globals :: M.Map Word16 BytecodeValue
}

callFrameFromChunk :: Chunk -> CallFrame
callFrameFromChunk chunk = CallFrame (Function chunk M.empty) M.empty 0

envFromBytecode :: Program -> Env
envFromBytecode (Program chunk) = Env [] (callFrameFromChunk chunk) [] M.empty

type VM a = StateT Env IO a

vmInit :: VM ()
vmInit = return ()

newStackFrame :: Function -> VM ()
newStackFrame function = do
    prev <- currentCallFrame <$> get
    let frame = CallFrame function M.empty 0
    modify' (\env -> env { prevCallFrames = prev : prevCallFrames env, currentCallFrame = frame })

getConstant :: Int -> VM BytecodeValue
getConstant i = do
    state <- get
    callFrame <- currentCallFrame <$> get
    let constants = constantsPool $ chunk $ function' callFrame
    return $! constants V.! i

{-# INLINE fetchOpcode #-}
fetchOpcode :: VM Opcode
fetchOpcode = toEnum . fromEnum <$> fetchByte

{-# INLINE fetchByte #-}
fetchByte :: VM Word8
fetchByte = do
    !ip <- getIP
    !callFrame <- currentCallFrame <$> get
    let opcodes = (code . chunk . function') $! callFrame
    incIP
    return $! opcodes U.! ip

fetchWord16 :: VM Word16
fetchWord16 = do
    !byte <- fromEnum . (`shiftL` 8) <$> fetchByte
    !byte' <- fromEnum <$> fetchByte
    return $! toEnum (byte + byte')

modifyCallFrame :: (CallFrame -> CallFrame) -> VM ()
modifyCallFrame !f = modify' (\env -> env {currentCallFrame = f (currentCallFrame env)})

modifyLocals :: (LocalVariables -> LocalVariables) -> VM ()
modifyLocals f = modifyCallFrame (\frame -> frame { locals = f (locals frame) })

addLocal :: Int -> BytecodeValue -> VM ()
addLocal x value = modifyLocals (M.insert x value)

getLocals :: VM LocalVariables
getLocals = locals <$> getCallFrame

getUpvalues :: VM (M.Map Int BytecodeValue)
getUpvalues = funcUpvalues . function' <$> getCallFrame

getCallFrame :: VM CallFrame
getCallFrame = do
    env <- get
    return $! (currentCallFrame env)

{-# INLINE getIP #-}
getIP :: VM Int
getIP = do
    -- !env <- get
    -- let !callFrame = currentCallFrame env
    !callFrame <- currentCallFrame <$> get
    let !ip' = ip callFrame
    return $! ip'

{-# INLINE incIP #-}
incIP :: VM ()
incIP = modifyIP $! (+1)

{-# INLINE modifyIP #-}
modifyIP :: (Int -> Int) -> VM ()
modifyIP !f = modifyCallFrame (\frame -> frame { ip = f $! (ip frame) })

modifyStack :: ([BytecodeValue] -> [BytecodeValue]) -> VM ()
modifyStack f = modify' (\env -> env { stack = f (stack env) })

removeStackFrame :: VM ()
removeStackFrame = do
    frames <- prevCallFrames <$> get
    let prev = head frames
    modify' (\env -> env { prevCallFrames = tail frames, currentCallFrame = prev })

modifyGlobals :: (M.Map Word16 BytecodeValue -> M.Map Word16 BytecodeValue) -> VM ()
modifyGlobals f = modify' (\env -> env { globals = f (globals env) })

addVariable :: (Word16, BytecodeValue) -> VM ()
addVariable (var, val) = modifyGlobals (M.insert var val)

getVariable :: Word16 -> VM BytecodeValue
getVariable var = do
    env <- get
    case M.lookup var (globals env) of
        Just v -> return $! v
        Nothing -> error "variable not found"

getStack :: VM [BytecodeValue]
getStack = stack <$> get

push :: BytecodeValue -> VM ()
push v = modifyStack (v:)

pop :: VM BytecodeValue
pop = do
    top <- head <$> getStack
    modifyStack tail
    return $! top

getTop :: VM BytecodeValue
getTop = head <$> getStack

getInt :: BytecodeValue -> Maybe Int
getInt (BInt x) = Just x
getInt _ = Nothing

getBool :: BytecodeValue -> Maybe Bool
getBool (BBool b) = Just b
getBool _ = Nothing

getFunction :: BytecodeValue -> Maybe Function
getFunction (Func function) = Just function
getFunction _ = Nothing

popInt :: VM Int
popInt = do
    top <- pop
    return $! fromJust $ getInt top

popBool :: VM Bool
popBool = do
    top <- pop
    return $ fromJust $ getBool top

popFunction :: VM Function
popFunction = do
    top <- pop
    return $ fromJust $ getFunction top

getTopBool :: VM Bool
getTopBool = do
    top <- getTop
    return $ fromJust $ getBool top

binOp :: (Int -> Int -> Int) -> VM ()
binOp !f = do
    x <- popInt
    y <- popInt
    push $ BInt (f y x)

binOpCompare :: (BytecodeValue -> BytecodeValue -> Bool) -> VM ()
binOpCompare f = do
    x <- pop
    y <- pop
    push $ BBool (f y x)

execUnboxed :: Word8 -> VM ()
-- LOAD_CONST
execUnboxed 4 = do
    i <- fetchWord16
    constant <- getConstant (fromEnum i)
    push constant
-- STORE_NAME
execUnboxed 6 = do
    x <- pop
    w <- fetchWord16
    addVariable (w, x)
-- LOAD_NAME
execUnboxed 7 = fetchWord16 >>= getVariable >>= push
-- ADD
execUnboxed 0 = binOp (+)
-- LESS
execUnboxed 20 = binOpCompare (<)
-- PRINT
execUnboxed 5 = do
    x <- pop
    printValue x
-- JUMP_FALSE
execUnboxed 15 = do
    x <- popBool
    offset <- fromEnum <$> fetchWord16
    unless x (modifyIP (const offset))
-- JUMP_TRUE
execUnboxed 14 = do
    x <- popBool
    offset <- fromEnum <$> fetchWord16
    when x (modifyIP (const offset))
-- JUMP
execUnboxed 13 = fromEnum <$> fetchWord16 >>= \offset -> modifyIP (const offset)
-- POP
execUnboxed 24 = void pop

exec :: Opcode -> VM ()
exec LOAD_CONST = do
    i <- fetchWord16
    constant <- getConstant (fromEnum i)
    push constant
exec !STORE_NAME = do
    x <- pop
    w <- fetchWord16
    addVariable (w, x)
exec !LOAD_NAME = fetchWord16 >>= getVariable >>= push
exec STORE_FAST = do
    n <- fromEnum <$> fetchWord16
    x <- pop
    modifyLocals (M.insert n x)
exec LOAD_FAST = do
    n <- fromEnum <$> fetchWord16
    locals <- getLocals
    let value = locals M.! n
    push value
exec LOAD_UPVALUE = do
    n <- fromEnum <$> fetchWord16
    upvalues <- getUpvalues
    let value = upvalues M.! n
    push value
exec MUL = binOp (*)
exec ADD = binOp (+)
exec SUB = binOp (-)
exec DIV = binOp div
exec EQUAL = binOpCompare (==)
exec NOT_EQUAL = binOpCompare (/=)
exec GREATER = binOpCompare (>)
exec GREATER_EQUAL = binOpCompare (>=)
exec LESS = binOpCompare (<)
exec LESS_EQUAL = binOpCompare (<=)
exec PRINT = do
    x <- pop
    printValue x
exec !JUMP_FALSE = do
    x <- popBool
    offset <- fromEnum <$> fetchWord16
    unless x (modifyIP (const offset))
exec !JUMP_TRUE = do
    x <- popBool
    offset <- fromEnum <$> fetchWord16
    when x (modifyIP (const offset))
exec !JUMP = fromEnum <$> fetchWord16 >>= \offset -> modifyIP (const offset)
exec POP = void pop
exec DUPE_LAST = getTop >>= push
exec MAKE_CLOSURE = do
    f <- popFunction
    n <- fetchWord16
    vals <- replicateM (fromEnum n) $ do
        index <- fromEnum <$> fetchByte
        isLocal <- (== 1) <$> fetchByte
        return (Upvalue index isLocal)
    function <- foldM addUpvalueClosure f vals
    push (Func function)
exec CALL = do
    x <- fetchByte
    values <- reverse <$> replicateM (fromEnum x) pop
    function <- popFunction
    newStackFrame function
    forM_ (zip [0..] values) $ uncurry addLocal
exec RET = do
    retVal <- pop
    removeStackFrame
    push retVal
exec other = do
    error $ "not implemented: " ++ show other

data Upvalue = Upvalue {
    upvalueIndex :: Int,
    isLocal :: Bool
} deriving (Show, Eq, Ord)

addUpvalueClosure :: Function -> Upvalue -> VM Function
addUpvalueClosure func (Upvalue upvalue isLocal) = do
    if isLocal then do
        localVars <- locals <$> getCallFrame
        let value = localVars M.! upvalue
        let upvalues = funcUpvalues func
        let newUpvalues = M.insert upvalue value upvalues
        let newFunc = func { funcUpvalues = newUpvalues }
        return newFunc
    else do
        upvalues <- getUpvalues
        let value = upvalues M.! upvalue
        let upvalues' = funcUpvalues func
        let newUpvalues = M.insert upvalue value upvalues
        let newFunc = func { funcUpvalues = newUpvalues }
        return newFunc

printValue :: BytecodeValue -> VM ()
printValue = \case
    BInt x -> liftIO $ print x
    BString s -> liftIO $ print s
    Func f -> liftIO $ putStrLn "<fn>"
    x -> liftIO $ print x

debugExecuteBytecode :: VM ()
debugExecuteBytecode = do
    opcode <- fetchOpcode
    liftIO $ print opcode
    case opcode of
        EXIT -> return ()
        _ -> do
            exec opcode
            debugExecuteBytecode

executeBytecode = executeUnboxed
-- executeBytecode :: VM ()
-- executeBytecode = do
--     !opcode <- fetchOpcode
--     case opcode of
--         EXIT -> return ()
--         _ -> do
--             exec $! opcode
--             executeBytecode

executeUnboxed :: VM ()
executeUnboxed = do
    !byte <- fetchByte
    case byte of
        28 -> return () -- EXIT Opcode
        _ -> do
            execUnboxed $! byte
            executeUnboxed

debugProgram :: Program -> IO ()
debugProgram program = do
    void $ (execStateT debugExecuteBytecode (envFromBytecode program))

runExecProgram :: Program -> IO ()
runExecProgram !program = do
    void $! (execStateT executeBytecode  (envFromBytecode program))

-- runExecBytecode :: Bytecode -> IO ()
-- runExecBytecode bytecode = do
--     result <- runExceptT (execStateT executeBytecode (envFromBytecode bytecode))
--     case result of
--         Left s -> putStrLn s
--         Right a -> putStr ""--print $ stack a