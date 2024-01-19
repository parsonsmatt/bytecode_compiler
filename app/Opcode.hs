module Opcode (Opcode (..)) where

import Data.Word
import Data.Bits

data Opcode = ADD
    | SUB
    | MUL
    | DIV
    | LOAD_CONST
    | PRINT
    | STORE_NAME
    | LOAD_NAME
    | STORE_FAST
    | LOAD_FAST
    | STORE_UPVALUE
    | LOAD_UPVALUE
    | MAKE_CLOSURE
    | JUMP
    | JUMP_TRUE
    | JUMP_FALSE
    | EQUAL
    | NOT_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    | AND
    | OR
    | POP
    | DUPE_LAST
    | CALL
    | RET
    | EXIT
    deriving (Enum, Show, Eq, Ord)

enumToEnum :: (Enum a, Enum b) => a -> b
enumToEnum = toEnum . fromEnum

word16ToWord8s :: Word16 -> [Word8]
word16ToWord8s word =
    let byte = enumToEnum $ word `shiftR` 8
        byte' = enumToEnum $ word .&. 0xff
    in [byte, byte']

-- opcodeToBytes :: Opcode -> [Word8]
-- opcodeToBytes opcode = case opcode of
--     ADD -> [0]
--     SUB -> [1]
--     MUL -> [2]
--     DIV -> [3]
--     LOAD_CONST bytes -> 4 : word16ToWord8s bytes
--     PRINT -> [5]
--     STORE_NAME -> [6]
--     LOAD_NAME -> [7]
--     STORE_FAST byte -> [8, byte]
--     LOAD_FAST byte -> [9, byte]
--     STORE_UPVALUE -> [10]
--     LOAD_UPVALUE -> [11]
--     MAKE_CLOSURE -> [12]
--     JUMP bytes -> 13 : word16ToWord8s bytes
--     JUMP_TRUE bytes -> 14 : word16ToWord8s bytes
--     JUMP_FALSE bytes -> 15 : word16ToWord8s bytes
--     EQUAL -> [16]
--     NOT_EQUAL -> [17]
--     GREATER -> [18]
--     GREATER_EQUAL -> [19]
--     LESS -> [20]
--     LESS_EQUAL -> [21]
--     AND -> [22]
--     OR -> [23]
--     POP -> [24]
--     DUPE_LAST -> [25]
--     CALL -> [26]
--     RET -> [27]
--     EXIT -> [28]

-- data Opcode' = ADD
-- data Opcode = ADD
--     | SUB
--     | MUL
--     | DIV
--     | LOAD_CONST Int
--     | PRINT
--     | STORE_NAME String
--     | LOAD_NAME String
--     | STORE_FAST Int
--     | LOAD_FAST Int
--     | STORE_UPVALUE Int
--     | LOAD_UPVALUE Int
--     | MAKE_CLOSURE [(Int, Int)]
--     | JUMP Int
--     | JUMP_TRUE Int
--     | JUMP_FALSE Int
--     | EQUAL
--     | NOT_EQUAL
--     | GREATER
--     | GREATER_EQUAL
--     | LESS
--     | LESS_EQUAL
--     | AND
--     | OR
--     | POP
--     | DUPE_LAST
--     | CALL Int
--     | RET
--     | EXIT
--     deriving (Show, Eq, Ord)