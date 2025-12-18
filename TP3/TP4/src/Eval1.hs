module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap, Monad (return)
                                                )
import Data.Bool (Bool(True))

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado:
newtype State a = State { runState :: Env -> Pair a Env }

-- def return y >>=
instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

    -- Ejercicio 1.a: Realizado en un archivo aparte --

    -- Ejercicio 1.b: Implementar el evaluador utilizando la monada State --

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, 
-- hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
-- Completar la definición
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip 
stepComm (Let v x) = evalExp x >>= update v >> return Skip
stepComm (Seq c1 c2) = stepComm c1 >> stepComm c2 
stepComm (IfThenElse b c1 c2) = do p <- evalExp b
                                   if p then stepComm c1
                                        else stepComm c2
stepComm c@(Repeat b c1) = do p <- evalExp b
                              if p then stepComm c1 >> stepComm c 
                                   else stepComm Skip

-- Aplica una operacion binaria a dos monad values
liftAaux :: Applicative f => (a->b->c) -> f a -> f b -> f c
liftAaux f x = (<*>) (fmap f x)

-- Evalua a una expresion constante
evalConst :: MonadState m => a -> m a
evalConst = return

-- Evalua una operacion unaria sobre una variable entera, 
-- obteniendo la actualización de estado de la misma
evalVarOp :: MonadState m => (Int->Int) -> Variable -> m Int
evalVarOp op v = do {
                  x <- lookfor v;
                  let x' = op x in do
                    ifThen2 (x/=x') (update v x');
                    return x';
                 }
    where
      ifThen2 :: Applicative f => Bool -> f () -> f ()
      ifThen2 pred expr = if pred then expr else pure ()

-- Toma una operacion unaria, una expresion y devuelve la evaluacion la operacion sobre la expresion
evalOpUnaria :: MonadState m => (a->b) -> Exp a -> m b
evalOpUnaria op = (fmap op) . evalExp

-- Toma una operacion binaria y dos expresiones y evalua la operacion sobre las expresiones
evalOpBinaria :: MonadState m => (a->a->b) -> Exp a -> Exp a -> m b
evalOpBinaria op x y = liftAaux op (evalExp x) (evalExp y)

evalExp :: MonadState m => Exp a -> m a
--Int
evalExp (Const a) = evalConst a
evalExp (Var v) = evalVarOp id v
evalExp (VarInc v)= evalVarOp (+1) v
evalExp (UMinus x) = evalOpUnaria negate x
evalExp (Plus x y) = evalOpBinaria (+) x y
evalExp (Minus x y) = evalOpBinaria (-) x y
evalExp (Times x y) = evalOpBinaria (*) x y
evalExp (Div x y) = evalOpBinaria (div) x y

--Bool
evalExp BTrue = evalConst True
evalExp BFalse = evalConst False
evalExp (Lt x y) = evalOpBinaria (<) x y
evalExp (Gt x y) = evalOpBinaria (>) x y
evalExp (And x y) = evalOpBinaria (&&) x y
evalExp (Or x y) = evalOpBinaria (||) x y
evalExp (Not x) = evalOpUnaria not x
evalExp (Eq x y) = evalOpBinaria (==) x y
evalExp (NEq x y) = evalOpBinaria (/=) x y

