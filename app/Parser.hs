{-# LANGUAGE OverloadedRecordDot #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Functor
import qualified Data.Set.Internal as E

import Ast
import Data.Maybe (fromJust)
import Debug.Trace
import Control.Monad
import Data.List (singleton)

data ErrType = NoTypeForParam String
    | TrailingCommaInFuncArgs
    | ExpectedReturnType
    | NoOpeningBrace
    | NoClosingBrace
    | NoCommaBeforeParam
    | ConditionNotSupplied String
    deriving (Eq, Show, Ord)

data Err = Err {errType :: ErrType, errLength :: Int}
    deriving (Eq, Show, Ord)

throwError :: Int -> ErrType -> Int -> Parser ()
throwError o t n = registerParseError (FancyError o (E.singleton (ErrorCustom $ Err t n)))

instance ShowErrorComponent Err where
    showErrorComponent a = case errType a of
        (NoTypeForParam s) -> "parameter `" ++ s ++ "` has not been given a type in function"
        TrailingCommaInFuncArgs -> "it appears you have an extra comma in your function arguments 😕"
        ExpectedReturnType -> "looks like you forgot to add the return type silly 🤪🤪"
        NoOpeningBrace -> "You forgot to add an opening bracket for your function. How is the function going to know what code to execute then? 😁"
        NoClosingBrace -> "You need a closing brace for your function."
        NoCommaBeforeParam -> "There is no comma separating this parameter from the previous one."
        ConditionNotSupplied s -> "You haven't supplied a condition for the parameter '" ++ s ++ "'."
    errorComponentLen = errLength

type Parser = Parsec Err String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    empty

parseInt :: Parser Int
parseInt = do
    int <- some digitChar
    notFollowedBy letterChar
    return $ read int

parseString :: Parser String
parseString = char '\"' >> manyTill L.charLiteral (char '\"')

parseBool :: Parser Bool
parseBool = (string "True" $> True) <|> (string "False" $> False)

parseValue :: Parser Value
parseValue = Int <$> parseInt
    <|> String <$> parseString
    <|> Bool <$> parseBool

parseWithPos :: Parser (Position -> a) -> Parser a
parseWithPos p = do
    offset <- getOffset
    x <- p
    len <- getOffset <&> subtract offset
    return $ x (Position offset len)

parsePrimary :: Parser Exp
parsePrimary = lexeme $ parseWithPos (Exp <$> (do
    Val <$> parseValue
    <|> Var <$> parseIdentifier))
    <|> (do
        lexeme (char '(')
        expr <- parseExp
        lexeme (char ')')
        return expr)

funcCall :: Parser [Exp]
funcCall = do
    lexeme (char '(')
    args <- lexeme parseArgs
    char ')'
    return args
    where
        parseArgs :: Parser [Exp]
        parseArgs = (do
            first <- parseExp
            rest <- many $ do
                lexeme (char ',')
                parseExp
            return (first:rest)) <|> return []

parseCallFunc :: Parser Exp
parseCallFunc = do
    primary <- parsePrimary
    let pos = primary.expPos
    flattenedCalls primary (posOffset primary.expPos)
    where
        flattenedCalls :: Exp -> Int -> Parser Exp
        flattenedCalls exp offset = (do
            op <- funcCall <&> CallFunc exp
            len <- getOffset <&> subtract offset
            flattenedCalls (Exp op (Position offset len)) offset
            ) <|> return exp

parseFactor :: Parser Exp
parseFactor = do
    offset <- getOffset
    v <- lexeme parseCallFunc
    flattenedFactor offset v
  where
    flattenedFactor :: Int -> Exp -> Parser Exp
    flattenedFactor offset v = (do
        op <- lexeme ((char '*' $> Mul) <|> (char '/' $> Div))
        v1 <- parseCallFunc
        let p = expPos v1
        let len = (posOffset p + posLength p) - offset
        flattenedFactor offset (Exp (op v v1) (Position offset len))) <|> return v

parseTerm :: Parser Exp
parseTerm = do
    offset <- getOffset
    v <- parseFactor
    flattenedTerm offset v
  where
    flattenedTerm :: Int -> Exp -> Parser Exp
    flattenedTerm offset v = (do
        op <- lexeme $ (char '+' $> Add) <|> (char '-' $> Sub)
        v1 <- parseFactor
        let p = expPos v1
        let len = (posOffset p + posLength p) - offset
        flattenedTerm offset (Exp (op v v1) (Position offset len))) <|> return v

parseComparison :: Parser Exp
parseComparison = do
    offset <- getOffset
    t <- parseTerm
    (do
        op <- lexeme ((string ">=" $> GreaterEqual)
                <|> (string ">" $> Greater)
                <|> (string "<=" $> LessEqual)
                <|> (string "<" $> Less))
        t1 <- parseTerm
        let p = expPos t1
        let len = (posOffset p + posLength p) - offset
        return (Exp (op t t1) (Position offset len))) <|> return t

parseEquality :: Parser Exp
parseEquality = do
    offset <- getOffset
    c <- parseComparison
    (do
        op <- lexeme (string "==" $> Equal) <|> (string "!=" $> NotEqual)
        e1 <- parseComparison
        let p = expPos e1
        let len = (posOffset p + posLength p) - offset
        return (Exp (op c e1) (Position offset len))) <|> return c

parseBoolOp :: Parser Exp
parseBoolOp = do
    offset <- getOffset
    e <- parseEquality
    flattenedBool offset e
    where
        flattenedBool :: Int -> Exp -> Parser Exp
        flattenedBool offset e = do
            op <- lexeme ((string "&&" $> And) <|> (string "||" $> Or))
            e1 <- parseEquality
            let p = expPos e1
            let len = (posOffset p + posLength p) - offset
            flattenedBool offset (Exp (op e e1) (Position offset len))
            <|> return e

parseLambda :: Parser Exp
parseLambda = (do
    offset <- getOffset
    lexeme (char '\\' <|> char 'λ')
    params <- parseParams
    lexeme (string "->")
    exp <- parseExp
    let p = expPos exp
    let len = (posOffset p + posLength p) - offset
    return $ Exp (Lambda params exp) (Position offset len)) <|> parseBoolOp
    where
        parseParams :: Parser [(String, Position)]
        parseParams = do
            first <- lexeme (parseAndPosition parseIdentifier)
            (do
                rest <- many $ lexeme (parseAndPosition parseIdentifier)
                return $ first:rest)

parseIf :: Parser Exp
parseIf = (do
    offset <- getOffset
    keyword "if"
    cond <- lexeme parseExp
    exp <- parseScope'
    keyword "else"
    expElse <- parseScope'
    len <- getOffset <&> subtract offset
    return $ Exp (If cond exp expElse) (Position offset len)) <|> parseLambda

parseStaticType :: Parser Exp
parseStaticType = (do
    offset <- getOffset
    keyword "staticType"
    exp <- parseExp
    len <- getOffset <&> subtract offset
    return $ Exp (StaticType exp) (Position offset len)) <|> parseIf

parseExp :: Parser Exp
parseExp = parseStaticType

keyword :: String -> Parser String
keyword s = lexeme $ try (string s <* notFollowedBy alphaNumChar)

parsePrint :: Parser Stmt
parsePrint = do
    offset <- getOffset
    keyword "print"
    exp <- parseExp
    let pos = expPos exp
    let len = (posOffset pos + posLength pos) - offset
    return $ Stmt (Print exp) (Position offset len)

parseIdentifier :: Parser String
parseIdentifier = do
    first <- letterChar
    rest <- many (letterChar <|> digitChar <|> char '_')
    return (first:rest)

parseVarAssign' :: Parser Stmt
parseVarAssign' = do
    offset <- getOffset
    t <- (keyword "var" $> VarAssign) <|> (keyword "let" $> LetAssign)
    iden <- lexeme parseIdentifier
    lexeme (char '=')
    exp <- parseExp
    let pos = expPos exp
    let len = (posOffset pos + posLength pos) - offset
    return $ Stmt (t iden exp) (Position offset len)

parseVarReassign' :: Parser Stmt
parseVarReassign' = do
    offset <- getOffset
    iden <- lexeme parseIdentifier
    lexeme (char '=')
    exp <- parseExp
    let pos = expPos exp
    let len = (posOffset pos + posLength pos) - offset
    return $ Stmt (VarReassign iden exp) (Position offset len)

parseVarAssign :: Parser Stmt
parseVarAssign = do
    offset <- getOffset
    t <- (keyword "var" $> VarAssign) <|> (keyword "let" $> LetAssign)
    iden <- lexeme parseIdentifier
    lexeme (char '=')
    exp <- parseExp
    let pos = expPos exp
    let len = (posOffset pos + posLength pos) - offset
    return $ Stmt (t iden exp) (Position offset len)

parseScope :: Parser [Stmt]
parseScope = do
    lexeme (char '{')
    stmts <- many (lexeme parseStmt)
    sc
    offset <- getOffset
    parseWithError (NoClosingBrace) (Position offset 1) (()) (lexeme (void $ char '}'))
    return stmts

parseScope' :: Parser Exp
parseScope' = do
    lexeme (char '{')
    exp <- lexeme parseExp
    sc
    offset <- getOffset
    parseWithError NoClosingBrace (Position offset 1) () (lexeme (void $ char '}'))
    len <- getOffset <&> subtract offset
    return exp

parseWhile :: Parser Stmt
parseWhile = do
    offset <- getOffset
    keyword "while"
    expStmt <- lexeme parseExp
    scope <- parseScope
    len <- getOffset <&> subtract offset
    return $ Stmt (While expStmt scope) (Position offset len)

parseFunctionType :: Parser Type
parseFunctionType = do
    lexeme (char '(')
    args <- parseArgs
    FUNCTION args <$> parseReturn
  where
    parseArg :: Parser Type
    parseArg = lexeme parseType
    parseArgs = (lexeme (char ')') $> []) <|> do
        first <- parseArg
        rest <- many (char ',' >> parseArg)
        lexeme (char ')')
        return (first:rest)
    parseReturn :: Parser Type
    parseReturn = lexeme (string "->") >> parseType

parseParenType :: Parser Type
parseParenType = do
    lexeme (char '(')
    args <- parseArgs
    (FUNCTION args <$> parseReturn) <|> return VOID
  where
    parseArg :: Parser Type
    parseArg = lexeme parseType
    parseArgs = (lexeme (char ')') $> []) <|> do
        first <- parseArg
        rest <- many (char ',' >> parseArg)
        lexeme (char ')')
        return (first:rest)
    parseReturn :: Parser Type
    parseReturn = lexeme (string "->") >> parseType

parseType :: Parser Type
parseType = (do
    lexeme (char '(')
    first <- parseType
    (do
        rest <- some $ (lexeme (char ',') >> parseType)
        lexeme (char ')')
        lexeme (string "->")
        r <- parseType
        return (FUNCTION (first:rest) r)
        ) <|> (do
        lexeme (char ')')
        (do
            lexeme (string "->")
            r <- parseType
            return (FUNCTION [first] r)) <|> return first
        )
    ) <|> (do
        t <- atomic
        (do
            lexeme (string "->")
            r <- parseType
            return (FUNCTION [t] r)) <|> return t)
  where
    atomic :: Parser Type
    atomic = (keyword "Int" $> INT)
        <|> (keyword "Double" $> DOUBLE)
        <|> (keyword "String" $> STRING)
        <|> (keyword "Bool" $> BOOL)

parseAndPosition :: Parser a -> Parser (a, Position)
parseAndPosition p = do
    offset <- getOffset
    r <- p
    len <- getOffset <&> subtract offset
    return (r, Position offset len)

parseWithError :: ErrType -> Position -> a -> Parser a -> Parser a
parseWithError errType pos a p = p <|> (throwError (posOffset pos) errType (posLength pos) $> a)

parseFuncDef :: Int -> String -> Parser Stmt
parseFuncDef offset iden = do
    lexeme (string "::")
    lexeme (char '(')
    args <- parseArgs
    t <- parseReturnType
    cases <- try (some $ parseCase iden (map fst args)) <|> (fail "missing function binding")
    len <- getOffset <&> subtract offset
    return $ Stmt (FuncDef iden args t cases) (Position offset len)
  where
    parseCase :: String -> [String] -> Parser FuncCase
    parseCase s args = do
        lexeme (string s)
        lexeme (char '(')
        args' <- parseArgs
        lexeme (char '=')
        scope <- parseScope <|> singleton <$> (\exp -> Stmt (ReturnStmt exp) posInit) <$> parseExp
        return $ FuncCase args' scope
      where
        parseArg :: Parser (String, Maybe Exp)
        parseArg = do
            (iden, idenPos) <- lexeme $ parseAndPosition parseIdentifier
            exp <- optional $ do
                lexeme (char ':')
                parseWithError (ConditionNotSupplied iden) idenPos Nothing (Just <$> parseExp)
            return (iden, join exp)
        parseArgs :: Parser [(String, Maybe Exp)]
        parseArgs = (lexeme (char ')') $> []) <|> do
            first <- parseArg
            rest <- many $ (do
                idenPos <- snd <$> lexeme (parseAndPosition (char ','))
                parseWithError TrailingCommaInFuncArgs idenPos ("", Nothing) parseArg) <|> (do
                    (iden, idenPos) <- lexeme $ parseAndPosition parseArg
                    parseWithError NoCommaBeforeParam idenPos ("", Nothing) parseArg
                )
            offset <- getOffset
            lexeme (char ')')
            return (first:rest)
    parseArg :: Parser (String, Type)
    parseArg = do
        (iden, idenPos) <- lexeme $ parseAndPosition parseIdentifier
        let withError = parseWithError (NoTypeForParam iden) idenPos (iden, VOID)
        withError $ do
            lexeme (char ':')
            withError (parseType >>= \t -> return (iden, t))

    parseArgs :: Parser [(String, Type)]
    parseArgs = (lexeme (char ')') $> []) <|> do
        first <- parseArg
        rest <- many $ (do
            idenPos <- snd <$> lexeme (parseAndPosition (char ','))
            parseWithError TrailingCommaInFuncArgs idenPos ("", VOID) parseArg) <|> (do
                (iden, idenPos) <- lexeme $ parseAndPosition parseIdentifier
                parseWithError NoCommaBeforeParam idenPos ("", VOID) parseArg
            )
        offset <- getOffset
        lexeme (char ')')
        return (first:rest)
    parseReturnType :: Parser Type
    parseReturnType = (do
        keyword "->"
        offset <- getOffset
        parseWithError ExpectedReturnType (Position offset 1) VOID parseType
        ) <|> return VOID

parseVarReassign :: Int -> String -> Parser Stmt
parseVarReassign offset iden = do
    lexeme (char '=')
    exp <- parseExp
    let pos = expPos exp
    let len = (posOffset pos + posLength pos) - offset
    return $ Stmt (VarReassign iden exp) (Position offset len)

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
    offset <- getOffset
    keyword "return"
    exp <- parseExp
    let pos = expPos exp
    let len = (posOffset pos + posLength pos) - offset
    return $ Stmt (ReturnStmt exp) (Position offset len)

parseCallExp :: Parser Stmt
parseCallExp = do
    exp <- parseExp
    let pos = expPos exp
    return $ Stmt (CallExp exp) pos

parseKeywordStmts :: Parser Stmt
parseKeywordStmts = do
    parsePrint
    <|> parseVarAssign
    <|> parseWhile
    <|> parseReturnStmt

parseIdenStmts :: Parser Stmt
parseIdenStmts = (do
    offset <- getOffset
    state <- getParserState
    iden <- lexeme parseIdentifier
    (parseFuncDef offset iden <|> parseVarReassign offset iden) <|> do
        setParserState state
        parseCallExp
    ) <|> parseCallExp

parseStmt :: Parser Stmt
parseStmt = do
    stmt <- parseKeywordStmts
        <|> parseIdenStmts
    return stmt

parseProgram :: Parser AST
parseProgram = do
    sc
    space
    stmts <- many (lexeme parseStmt)
    eof
    return stmts

forceParse :: String -> AST
forceParse = fromJust . parseMaybe parseProgram

parseEither :: String -> Either String AST
parseEither s = case parse parseProgram "error" s of
    Left err -> Left $ errorBundlePretty err
    Right ast -> Right ast

testParse' = parseTest parseProgram