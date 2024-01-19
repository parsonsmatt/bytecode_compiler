module CompilerError where

import Error.Diagnose hiding (Position)
import qualified Error.Diagnose as E
import Text.Megaparsec.Pos
import Data.List (singleton)

data CompileResultType = CompileError | CompileWarning

data CompileResultMsg = CompileResultMsg {
    rSpan :: (SourcePos, SourcePos),
    rString :: String,
    rNotes :: [String]
} deriving Show

data ResultContextMsg = ResultContextMsg {
    ctxSpan :: (SourcePos, SourcePos),
    ctxString :: String
} deriving Show

class CompileResult a where
    msgCode :: a -> Maybe String
    msgString :: a -> String
    resultType :: a -> CompileResultType
    resultMsg :: a -> CompileResultMsg
    ctxMsgs :: a -> [ResultContextMsg]

resultTypeToDiagnose :: CompileResultType
    -> Maybe String
    -> String
    -> [(E.Position, Marker String)]
    -> [Note String]
    -> Report String
resultTypeToDiagnose CompileError = err
resultTypeToDiagnose CompileWarning = warn

beautifyCompileResult :: CompileResult a => a -> Report String
beautifyCompileResult r =
    resultTypeToDiagnose (resultType r)
        (msgCode r)
        (msgString r)
        (convertResultMsg (resultMsg r) : convertCtxMsgs (ctxMsgs r))
        (map Note (rNotes $ resultMsg r))
  where
    convertResultMsg :: CompileResultMsg -> (E.Position, Marker String)
    convertResultMsg msg =
        (E.Position
            (unPos $ sourceLine start, unPos $ sourceColumn start)
            (unPos (sourceLine end), unPos (sourceColumn end))
            (sourceName start),
            This (rString msg)
        )
      where (start, end) = rSpan msg
    convertCtxMsgs :: [ResultContextMsg] -> [(E.Position, Marker String)]
    convertCtxMsgs = map $ \ctx -> let (start, end) = ctxSpan ctx in
        (E.Position
            (unPos $ sourceLine start, unPos $ sourceColumn start)
            (unPos (sourceLine end), unPos (sourceColumn end))
            (sourceName start),
            Where (ctxString ctx))