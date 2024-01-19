module Test where

import Bytecode
import Ast
import Opcode
import VM
import Parser
import Typechecker

import qualified Error.Diagnose as D
import System.IO
import Control.Monad.IO.Class
-- import Error.Diagnose.Compat.Megaparsec
-- import Error.Diagnose.Compat.Megaparsec

fromRight :: Either a b -> b
fromRight (Right b) =  b

-- testParse = forceParse "if 1 >= 1 { print 7 } else if 8 >= 0 { print 43 } else { print 5 } print 100"
-- testParse = forceParse "var x = 8 if x > 0 { print 100 } else { print 50 } print 25"
-- testParse = forceParse "var x = 5 while x > 0 { print x x = x - 1 }"
-- testParse = forceParse "var x = 8 x = 6 if False || True { if True { print x } }"

-- testParse = forceParse "func add(x, y) { var z = 12 func inner() {return z} return y - inner() } print add(5, 8)"
-- testParse = forceParse "var x = 8 func main() { var y = x func inner() { return y } return inner } \
--     \ var a = main() x = 20 var b = main() print a() print b()"
-- testParse = forceParse "var x = 8 func main() { var y = x func inner() { return y } return inner } \
--     \ var a = main()"
-- testParse = forceParse "func main() { var x = 3 func inner() { return x * x } return inner} var a = main()"
-- testParse = forceParse "func main() { var x = 3 func inner() {return x} return inner } var a = main() print a()"

-- testParse = forceParse "function fib(n: Int) { print n } var x = fib(4)"
-- typechecked' = fromRight $ typecheck testParse ""
-- compiled' = fromRight $ compileProgram typechecked'

-- disassembly = putStr $ disassembleProgram compiled'
-- testParse = forceParse "func f() {return f()} var x = f()"
-- testParse = forceParse "if True && False { print 5 } else { print 3 }"
-- testParse = forceParse "var x = f()"

-- testParse = forceParse "func main(n) {func fac(n) { if n == 0 {return 1} else {return n * fac(n-1)} } return fac(n)} print main()"

-- testParse = forceParse "func main() {func inner() {return inner()} return inner} var x = main() print x()"
-- testParse = forceParse "func add(x) {func g() { func i() {return x} return i} return g} print add(3)()()"

-- testParse = forceParse "func add(y) { var a = 3 var x = 9 func g() {func i() {func o() {return x} return o} return i} var b = 8 return g()} print add(4)()()"


-- testParse = forceParse "func main() {func inner() {return inner}}"
-- testParse = forceParse "func main() {return main()} print main()"

-- testParse = forceParse "func fib(n) { if n == 0 {return 0} else if n == 1 {return 1} else {return fib(n-1) + fib(n-2)}} print fib(12)"

-- testParse = forceParse "func main() { print x } print main()"
-- testParse = forceParse "func inner() { return 3 } print inner()"
-- testParse = forceParse "func square(x) { var x = 3 return x * x } print square(5)"

-- testParse = forceParse "func f() { return 5 } print f()"
-- testParse = forceParse "var x = 10 print x * 8"
-- testParse = forceParse "print 4"

-- testConvert = case typecheck testParse of
--     Left err -> Left $ show err
--     Right ast -> compileProgram ast

-- disassembly = putStr $ disassembleProgram (fromRight testConvert)

-- testOpcodes = print (code $ mainChunk $ fromRight testConvert)
-- testConstants = print (constantsPool $ mainChunk $ fromRight testConvert)
-- testFunctions = mapM_ (\x -> print x >> putStrLn "") (functions $ fromRight testConvert)
-- testRun = runExecProgram $ fromRight testConvert
-- debugTestRun = debugProgram $ fromRight testConvert

-- plainCode = "var x = 7 + 2 if 4 {print 8} print 2"

-- plainCode = "func f(x: Int) -> Bool { while False {return True} return True} print f(7)"
-- plainCode = "func f() -> Int {var x = 8 func g() -> Bool {var x = True return x} return x} print f() + f()"
-- plainCode = "function f(x   : Int     , y:Double) -> Int {return x * 8}"

exampleReport :: D.Report String
exampleReport =
  D.Err
    -- vv  OPTIONAL ERROR CODE
    Nothing
    -- (Just "what")
    -- vv  ERROR MESSAGE
    "This is my first error report"
    -- vv  MARKERS
    [ (D.Position (1, 3) (1, 8) "test.txt", D.This "Some text under the marker") ]
    -- vv  HINTS
    []

newDiagnostic :: D.Diagnostic String
newDiagnostic = D.addReport mempty exampleReport

dPrint :: Control.Monad.IO.Class.MonadIO m => m ()
dPrint = D.printDiagnostic stdout D.WithUnicode (D.TabSize 1) D.defaultStyle newDiagnostic

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Right c) = Right c
mapLeft f (Left a) = Left (f a)

-- typechecked' = do
--     case parseEither code >>= typecheck of
--         Right code -> print code
--         Left err -> putStrLn err
--   where
--     code = "var f = (\\x y -> x + 2)"

-- plainCode = "var f = (\\x y -> x + 3 + y) print f(3)"
plainCode = "var f = (\\x -> \\y -> x + y) print \"hello\""

-- plainCode = "var x = 2 print (x+True)"
-- plainCode = "function f(x, y) -> {return x + y} print f()"
parsePlain = testParse' plainCode

-- typechecked = do
--     ast <- parseEither plainCode
--     typecheck ast plainCode

-- compiled = typechecked >>= compileProgram

-- ran = case compiled of
--     Left err -> putStrLn err
--     Right program -> runExecProgram program