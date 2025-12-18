module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )
import Prelude hiding (fst,snd) --problemas de doble def
-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\e -> return (x :!: e))
  m >>= f = StateError (\e -> do (x :!: e') <- runStateError m e
                                 runStateError (f x) e')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw err = StateError (const (Left err))


-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case lookfor' v s  of
                                  Nothing -> Left UndefVar
                                  Just x' -> Right (x' :!: s)
                              )
    where lookfor' = M.lookup
  update v i = StateError (\s -> return (() :!: update' v i s)) 
    where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = runStateError (stepCommStar p) initEnv >>= (return . snd)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= stepCommStar

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip = return Skip 
stepComm (Let v x) = evalExp x >>= update v >> return Skip
stepComm (Seq c1 c2) = stepComm c1 >> stepComm c2 
stepComm (IfThenElse b c1 c2) = do p <- evalExp b
                                   if p then stepComm c1
                                        else stepComm c2
stepComm c@(Repeat b c1) = do p <- evalExp b
                              if p then stepComm c1 >> stepComm c 
                                   else stepComm Skip

-- Evalua a una expresion constante
evalConst :: (MonadState m, MonadError m) => a -> m a
evalConst = return

-- Evalua una operacion unaria sobre una variable entera, 
-- obteniendo la actualización de estado de la misma
evalVarOp :: (MonadState m, MonadError m) => (Int->Int) -> Variable -> m Int
evalVarOp op v = do {
                  x <- lookfor v;
                  let x' = op x in do
                    ifThen2 (x/=x') (update v x');
                    return x';
                 }
    where
      ifThen2 :: Applicative f => Bool -> f () -> f ()
      ifThen2 pred expr = if pred then expr else pure ()

-- Toma una operacion unaria y una expresion y evalua la operacion sobre la expresion
evalOpUnaria :: (MonadState m, MonadError m) => (a->b) -> Exp a -> m b
evalOpUnaria op = (fmap op) . evalExp

-- Funcion que chequea una condicion sobre 2 valores
type Check a m = a -> a -> m ()

-- Si se quiere hacer una division por 0, se lanza un error
checkDivByZero :: (MonadError m) => Check Int m
checkDivByZero _ y = ifThen2 (y==0) (throw DivByZero)
  where
    ifThen2 :: Applicative f => Bool -> f () -> f ()
    ifThen2 pred expr = if pred then expr else pure ()

-- Toma una condicion a chequear, una operacion y dos expresiones
-- Decide en caso de pasar el chequeo, evaluar la operacion sobre las expresiones
evalDivCheck :: (MonadState m, MonadError m) => Check a m -> (a->a->b) -> Exp a -> Exp a -> m b
evalDivCheck check op x y = do
                                    x' <- evalExp x
                                    y' <- evalExp y
                                    check x' y'
                                    return (op x' y')

-- Toma una operacion binaria y dos expresiones y evalua la operacion sobre las expresiones
evalOpBinaria :: (MonadState m, MonadError m) => (a->a->b) -> Exp a -> Exp a -> m b
evalOpBinaria op x y = do
                          x1 <- evalExp x
                          y1 <- evalExp y
                          return (op x1 y1)


evalExp :: (MonadState m, MonadError m) => Exp a -> m a
-- Int
evalExp (Const a) = evalConst a
evalExp (Var v) = evalVarOp id v
evalExp (VarInc v)= evalVarOp (+1) v
evalExp (UMinus x) = evalOpUnaria negate x
evalExp (Plus x y) = evalOpBinaria (+) x y
evalExp (Minus x y) = evalOpBinaria (-) x y
evalExp (Times x y) = evalOpBinaria (*) x y
-- ejercicio 2
evalExp (Div x y) = evalDivCheck checkDivByZero div x y

-- Bool
evalExp BTrue = evalConst True
evalExp BFalse = evalConst False
evalExp (Lt x y) = evalOpBinaria (<) x y
evalExp (Gt x y) = evalOpBinaria (>) x y
evalExp (And x y) = evalOpBinaria (&&) x y
evalExp (Or x y) = evalOpBinaria (||) x y
evalExp (Not x) = evalOpUnaria not x
evalExp (Eq x y) = evalOpBinaria (==) x y
evalExp (NEq x y) = evalOpBinaria (/=) x y


