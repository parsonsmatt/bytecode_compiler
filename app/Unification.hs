module Unification where

import TypeAnnotation hiding (Env, newEnv)
import Ast
import qualified Data.Set as S
import Control.Monad.State
import Data.Tuple.Extra

type SolutionSet = S.Set (TypeVariable, TypeVariable)
type ConstraintSet = S.Set (TypeVariable, TypeVariable)

replaceTypeVariable :: TypeVariable
    -> TypeVariable
    -> [(TypeVariable, TypeVariable)]
    -> [(TypeVariable, TypeVariable)]
replaceTypeVariable t1 t2 xs = map (both (substitute t1 t2)) xs
-- replaceTypeVariable t1 t2 [] = []
-- replaceTypeVariable t1 t2 ((a, b):xs)
--     | t1 == a = (t2, b) : replaceTypeVariable t1 t2 xs
--     | t1 == b = (a, t2) : replaceTypeVariable t1 t2 xs
--     | otherwise = (a, b) : replaceTypeVariable t1 t2 xs
  where
    substitute :: TypeVariable -> TypeVariable -> TypeVariable -> TypeVariable
    substitute t1 t2 a = case a of
        TVariable s -> if t1 == a then t2 else a
        Function xs y -> Function (map (substitute t1 t2) xs) (substitute t1 t2 y)
        Concrete x -> a

data Env = Env {
    solutions :: [(TypeVariable, TypeVariable)],
    remainingConstraints :: [(TypeVariable, TypeVariable)]
} deriving Show


addSolution :: Unifier ()
addSolution = do
    constraints <- remainingConstraints <$> get
    if null constraints then return ()
    else do
        let (constraint:rest) = constraints
        return ()

newEnv :: ConstraintSet -> Env
newEnv s = Env [] (S.toAscList s)

-- rewriteSolutionSet :: TypeVariable -> TypeVariable -> [(TypeVariable, TypeVariable)] -> [(TypeVariable, TypeVariable)]
-- rewriteSolutionSet t1 t2 =

-- aTest = mapM_ print $ addSolutions test'''

-- addSolutions :: S.Set (TypeVariable, TypeVariable) -> [(TypeVariable, TypeVariable)]
addSolutions s = return ()--execState (go list) (newEnv list)
  where
    list = S.toAscList s
    go :: Env -> [(TypeVariable, TypeVariable)] -> Env
    go env [] = env
    -- go ((t1, t2):xs) = do
    --     env <- get
    --     let new = replaceTypeVariable t1 t2 env
    --     put ((t1, t2) : new)
    --     go xs
        -- return ()

type Unifier a = State Env a

-- unify :: ConstraintSet -> SolutionSet
-- unify s = solutions (execState go (newEnv s))
--   where
--     go :: Unifier ()
--     go = return ()

-- unify :: ConstraintSet -> SolutionSet
-- unify s = S.empty
--   where
--     list = S.toAscList s
--     solutions = S.empty
-- unify :: S.Set (TypeVariable, TypeVariable) -> Either String (S.Set (TypeVariable, Type))
-- unify s = Right S.empty
--   where
--     list = S.toAscList s
    -- unfiy'' :: (TypeVariable, TypeVariable) -> (TypeVariable, TypeVariable) -> (TypeVariable, TypeVariable)
    -- unify''
    -- unify' :: (TypeVariable, TypeVariable) -> S.Set (TypeVariable, TypeVariable) -> S.Set (TypeVariable, TypeVariable)
    -- unify' (a, b) s = S.map (\(a', b') -> )