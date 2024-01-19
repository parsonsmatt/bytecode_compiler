{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}

module Main where

import System.Clock
import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Parser
import Typechecker
import Bytecode
import VM
import Text.Megaparsec
import Data.Functor
-- import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGR)
import System.Console.ANSI
import Data.Void
import qualified Data.List.NonEmpty as NE
import Error.Diagnose hiding (Position)
import qualified Error.Diagnose as E
import qualified Data.Set as S
import qualified Data.Map as M
import CompilerError
import Data.List
import Data.Maybe
import Ast
-- import Error.Diagnose.Compat.Megaparsec
import Control.DeepSeq

testParse :: IO ()
testParse = do
    let filename = "test.txt"
    file <- openFile filename ReadMode
    text <- hGetContents file
    case parse parseProgram "error" text of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> mapM_ print ast

testCompiledCode :: IO ()
testCompiledCode = do
    let filename = "test.txt"
    file <- openFile filename ReadMode
    text <- hGetContents file
    case parse parseProgram "error" text of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> case typecheck'' ast text filename of
            Right tast -> case compileProgram tast of
                Left err -> putStrLn err
                Right program -> print program
            Left errs -> do
                let reports = map beautifyCompileResult errs
                let diagnostic = addFile mempty filename text
                let diagnostic' = foldl addReport diagnostic reports
                printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'

testTypecheck :: IO ()
testTypecheck = do
    let filename = "test.txt"
    file <- openFile filename ReadMode
    text <- hGetContents file
    case parse parseProgram "error" text of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> case typecheck'' ast text filename of
            Right tast -> print tast
            Left errs -> do
                let reports = map beautifyCompileResult errs
                let diagnostic = addFile mempty filename text
                let diagnostic' = foldl addReport diagnostic reports
                printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'

testDisassemble :: IO ()
testDisassemble = do
    let filename = "test.txt"
    file <- openFile filename ReadMode
    text <- hGetContents file
    case parse parseProgram "error" text of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> case typecheck'' ast text filename of
            Right tast -> case compileProgram tast of
                Left err -> putStrLn err
                Right program -> putStr $ disassembleProgram program
            Left errs -> do
                let reports = map beautifyCompileResult errs
                let diagnostic = addFile mempty filename text
                let diagnostic' = foldl addReport diagnostic reports
                printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'

testRun :: IO ()
testRun = do
    let filename = "test.txt"
    file <- openFile filename ReadMode
    text <- hGetContents file
    case parse parseProgram "error" text of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> case typecheck'' ast text filename of
            Right tast -> case compileProgram tast of
                Left err -> putStrLn err
                Right program -> runExecProgram program
            Left errs -> do
                let reports = map beautifyCompileResult errs
                let diagnostic = addFile mempty filename text
                let diagnostic' = foldl addReport diagnostic reports
                printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'

testRunDebug :: IO ()
testRunDebug = do
    let filename = "test.txt"
    file <- openFile filename ReadMode
    text <- hGetContents file
    case parse parseProgram "error" text of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> case typecheck'' ast text filename of
            Right tast -> case compileProgram tast of
                Left err -> putStrLn err
                Right program -> debugProgram program
            Left errs -> do
                let reports = map beautifyCompileResult errs
                let diagnostic = addFile mempty filename text
                let diagnostic' = foldl addReport diagnostic reports
                printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'

-- outputCompiled :: IO ()
-- outputCompiled = do
--     let filename = "test.txt"
--     file <- openFile filename ReadMode
--     text <- hGetContents file
--     case parse parseProgram "error" text of
--         Left err -> putStrLn $ errorBundlePretty err
--         Right ast -> print (typecheck ast text >>= compileProgram)

-- disassemble :: IO ()
-- disassemble = do
--     let filename = "test.txt"
--     file <- openFile filename ReadMode
--     text <- hGetContents file
--     case parse parseProgram "error" text of
--         Left err -> putStrLn $ errorBundlePretty err
--         Right ast -> case (typecheck ast text >>= compileProgram <&> disassembleProgram) of
--             Left err -> putStrLn err
--             Right t -> putStrLn t

text = "fib :: (x: Int) -> Int\nfib(x) = 2\nprint fib(10)"

timed :: String -> IO a -> IO a
timed message action = do
    start <- getTime Monotonic
    a <- action
    end <- getTime Monotonic
    print (message <> " Action took: ", toNanoSecs (diffTimeSpec end start) `div` 1000, " milliseconds")
    pure a

main :: IO ()
main = timed "main" $ do
    args <- getArgs
    -- let filename = "test.txt"
    let filename = head args
    file <- openFile filename ReadMode
    text <- hGetContents file
    parseResult <- timed "parseProgram" $ do
        pure $!! parse parseProgram "error" text
    case parseResult of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast ->  do
            tast <- timed "typecheck''" $ do
                typechecked <- pure $! typecheck'' ast text filename
                case typechecked of
                    Right tast -> do
                        pure $!! tast
                    Left errs -> do
                        let reports = map beautifyCompileResult errs
                        let diagnostic = addFile mempty filename text
                        let diagnostic' = foldl addReport diagnostic reports
                        printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'
                        error "oh no"
            program <- timed "compileProgram" $ do
                compiled <- pure $! compileProgram tast
                case compiled of
                    Left err -> do
                        putStrLn err
                        error "oh no"
                    Right program -> pure $!! program
            timed "runExecProgram" $ runExecProgram program

-- main :: IO ()
-- main = do
--     args <- getArgs
--     let filename = head args
--     file <- openFile filename ReadMode
--     text <- hGetContents file
--     case parse parseProgram "error" text of
--         Left err -> putStrLn $ colorizeErrors err
--         Right ast -> case typecheck ast text of
--             Right tast -> case compileProgram tast of
--                 Left err -> putStrLn err
--                 Right program -> runExecProgram program
--             Left err -> print err--putStrLn $ errorBundlePretty err

colorizeErrors :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> String
colorizeErrors errs = unlines $ zipWith (\n l -> show n ++ ": " ++ colorizeError l) [1..] (lines $ errorBundlePretty errs)

colorizeError :: String -> String
colorizeError err = concat (zipWith (\n c -> "\ESC[" ++ show n ++ "m" ++ [c]) (cycle $ [31..36] ++ [91..96]) err) ++ "\ESC[m"

makePosState :: String -> a -> PosState a
makePosState filename s = PosState
    { pstateInput = s
    , pstateOffset = 0
    , pstateSourcePos = initialPos filename
    , pstateTabWidth = defaultTabWidth
    , pstateLinePrefix = ""
    }

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

annotateTypeErrorBundle :: ParseErrorBundle String TypeError -> NE.NonEmpty (ParseError String TypeError, SourcePos)
annotateTypeErrorBundle bundle = fst $
    attachSourcePos errorOffset
        (bundleErrors bundle)
        (bundlePosState bundle)

printColor = do
    putStr $ setSGRCode [SetColor Foreground Vivid Red]
    putStrLn "Hello"
    putStr $ setSGRCode [Reset]

getErrorCustom :: ErrorFancy e -> e
getErrorCustom (ErrorCustom e) = e
getErrorCustom _ = error "error jlkfajslfkjsda"

-- beautifyTypeErrs :: [((TypecheckNode', SourcePos), [(SupportNodeType, SourcePos)])] -> [Report String]
-- beautifyTypeErrs [] = []
-- beautifyTypeErrs (((n, s), y):xs) = case n of
--     ImmutableVarReassign s' -> beautifyErr' (show n, s) (map (\(x, z) -> (show x, z)) y) : beautifyTypeErrs xs

data SupportError = SupportError {
    sErrString :: String,
    sErrSourcePos :: SourcePos,
    sErrLength :: Int
}
-- data CustomError = CustomError {
--     errString :: String,
--     errSourcePos :: SourcePos,
--     errLength :: Int
-- }

-- typecheckNodeToErr :: TypecheckNode' -> (Maybe String, String, String, [String])
-- typecheckNodeToErr n = case n of
--     ImmutableVarReassign s -> (
--         Just "Reassign Immutable Variable",
--         "You have tried to mutate the variable \"" ++ s ++ "\" but it is immutable.",
--         "Tried to reassign here",
--         ["Perhaps you could make \"" ++ s ++ "\" mutable?"])

beautifyErr' :: (String, SourcePos) -> [(String, SourcePos)] -> Report String
beautifyErr' (s, pos) supporting =
    Error.Diagnose.Err
        (Just "reassign immutable var")
        s
        -- "Attempt to reassign immutable variable"
        ([
            (E.Position
                (unPos $ sourceLine pos, unPos $ sourceColumn pos)
                (unPos (sourceLine pos), 10 + unPos (sourceColumn pos))
                (sourceName pos),
                This "Tried to reassign here"
                )
            -- (E.Position (1, 25) (2, 6) "test.txt", This "While applying function '+'")
        ] ++ map (\(s, pos) -> (E.Position
                ((unPos $ sourceLine pos), unPos $ sourceColumn pos)
                (unPos (sourceLine pos), unPos (sourceColumn pos))
                (sourceName pos),
                Where s)) supporting)
        [Note "Perhaps you could make the variable mutable?"]

-- beautifyTypeErrors :: [(ParseError String TypeError, SourcePos)] -> String -> [Report String]
-- beautifyTypeErrors [] _ = []
-- beautifyTypeErrors ((p, sourcePos):xs) name = case p of
--     FancyError o errs ->
--         let x = getErrorCustom (S.findMin errs)
--         in case x.tErrType of
--             (ReassignImmutableVar s) -> beautifyErr (s, sourcePos) name x.offset : beautifyTypeErrors xs name
--             _ -> beautifyTypeErrors xs name

beautifyTypeErrs :: [TypeErrorSrc] -> [Report String]
beautifyTypeErrs = map beautifyErr
-- beautifyTypeErrs (x:xs) = case x._tErrType of

beautifyErr :: TypeErrorSrc -> Report String
beautifyErr t = case t._tErrType of
    ReassignImmutableVar s -> Error.Diagnose.Err
        -- Nothing
        (Just "reassign immutable var")
        "Attempt to reassign immutable variable"
        ([
            (E.Position
                (unPos $ sourceLine start, unPos $ sourceColumn start)
                (unPos $ sourceLine end, unPos $ sourceColumn end)
                (sourceName start),
                This "Tried to reassign here")
        ] ++ map contextToMarker contexts)
        [Note "Perhaps you could make the variable mutable?"]
  where
    (start, end) = t._src
    contextToMarker :: TypeErrorContextSrc -> (E.Position, Marker String)
    contextToMarker ctx = case ctx._ctxType of
        DeclaredHere -> (E.Position
            (unPos $ sourceLine start, unPos $ sourceColumn start)
            (unPos $ sourceLine end, unPos $ sourceColumn end)
            (sourceName start),
            Where "Declared Here")
      where
        (start, end) = _ctxSrc ctx
    contexts = _ctx t


-- beautifyErr :: (String, SourcePos) -> String -> Position -> Report String
-- beautifyErr (s, pos) name pos' =
--     err
--         (Just "reassign immutable var")
--         "Attempt to reassign immutable variable"
--         [
--             (E.Position
--                 (unPos $ sourceLine pos, unPos $ sourceColumn pos)
--                 (unPos (sourceLine pos), pos'.posLength + unPos (sourceColumn pos))
--                 name,
--                 This "While applying function"
--                 )
--             -- (E.Position (1, 25) (2, 6) "test.txt", This "While applying function '+'")
--         ]
--         [Note "Try doing x, y, and z"]

beautifulExample =
      Error.Diagnose.Err
        (Just "what")
        "Could not deduce constraint 'Num(a)' from the current context"
        [ (E.Position (1, 25) (2, 6) "test.txt", This "While applying function '+'"),
          (E.Position (1, 11) (1, 16) "test.txt", Where "'x' is supposed to have type 'a'"),
        --   (Position (1, 8) (1, 9) "somefile.zc", Maybe "try doing x, y, and z"),
          (E.Position (1, 8) (1, 9) "test.txt", Where "type 'a' is bound here without constraints")
        ]
        [Note "Adding 'Num(a)' to the list of constraints may solve this problem."]
--         ^^^^ This is a 'Note' not a 'Hint', as specified by its 'IsString' instance

-- Create the diagnostic
p :: IO ()
p = do
    let filename = "test.txt"
    file <- openFile filename ReadMode
    text <- hGetContents file
    let diagnostic = addFile mempty filename text
    let diagnostic' = addReport diagnostic beautifulExample
    printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'

-- diagnostic  = addFile mempty "somefile.zc" "let id<a>(x : a) : a := x\n  + 1"
-- diagnostic' = addReport diagnostic beautifulExample

-- Print with unicode characters, and the default (colorful) style
-- p :: IO ()
-- p = printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'
