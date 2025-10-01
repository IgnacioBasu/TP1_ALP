--Eval2
module Eval2
  ( eval
  , State
  , Error
  )
where

import AST
import qualified Data.Map.Strict as M
import Data.Strict.Tuple
import Control.Monad (Monad(return))

-- Estados
type State = M.Map Variable Int

-- Estado vacío
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
    Just n  -> Right n
    Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = Right s
stepCommStar c    s = do
    (c' :!: s') <- stepComm c s
    stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s = Right (Skip :!: s)
stepComm (Let v e) s = do
    (val :!: s1) <- evalExp e s
    return (Skip :!: update v val s1)
stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s = do
    (c1' :!: s1) <- stepComm c1 s
    return ((Seq c1' c2) :!: s1)
stepComm (IfThenElse b c1 c2) s = do
    (cond :!: s1) <- evalExp b s
    if cond then return (c1 :!: s1)
            else return (c2 :!: s1)
stepComm (RepeatUntil c b) s =
    let val = Seq c (IfThenElse b Skip (RepeatUntil c b))
    in Right (val :!: s)

-- Evalúa una expresión
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var v) s = do
    val <- lookfor v s
    return (val :!: s)
evalExp (UMinus e) s = do
    (v :!: s1) <- evalExp e s
    return ((-v) :!: s1)
evalExp (VarInc v) s = do
    val <- lookfor v s
    let newVal  = val + 1
        newState = update v newVal s
    return (newVal :!: newState)

evalExp (Plus e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 + v2) :!: s2)

evalExp (Minus e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 - v2) :!: s2)

evalExp (Times e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 * v2) :!: s2)

evalExp (Div e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    if v2 == 0
       then Left DivByZero
       else return ((v1 `div` v2) :!: s2)

evalExp BTrue s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)

evalExp (Lt e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 < v2) :!: s2)

evalExp (Gt e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 > v2) :!: s2)

evalExp (And b1 b2) s = do
    (v1 :!: s1) <- evalExp b1 s
    (v2 :!: s2) <- evalExp b2 s1
    return ((v1 && v2) :!: s2)

evalExp (Or b1 b2) s = do
    (v1 :!: s1) <- evalExp b1 s
    (v2 :!: s2) <- evalExp b2 s1
    return ((v1 || v2) :!: s2)

evalExp (Not b) s = do
    (v :!: s1) <- evalExp b s
    return ((not v) :!: s1)

evalExp (Eq b1 b2) s = do
    (v1 :!: s1) <- evalExp b1 s
    (v2 :!: s2) <- evalExp b2 s1
    return ((v1 == v2) :!: s2)

evalExp (NEq b1 b2) s = do
    (v1 :!: s1) <- evalExp b1 s
    (v2 :!: s2) <- evalExp b2 s1
    return ((v1 /= v2) :!: s2)
