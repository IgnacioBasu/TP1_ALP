module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
--import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|.
newtype StateErrorTrace a =
  StateErrorTrace { runStateErrorTrace :: Env -> (Either Error (a, Env), Trace) }


-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs
instance Monad StateErrorTrace where
  return x = StateErrorTrace (\e -> (return (x,e), ""))
  m >>= f = StateErrorTrace (\e -> let 
                                    (st,t) = runStateErrorTrace m e
                                   in case st of
                                        Left e -> (Left e, t)
                                        Right (a,e') -> let
                                                          (st', t') = runStateErrorTrace (f a) e'
                                                        in
                                                          (
                                                            st', if t=="" 
                                                                  then t' 
                                                                  else if t'=="" 
                                                                    then t 
                                                                    else t' ++" ,"++ t
                                                          ) 
                              )

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where 
  addTrace s = StateErrorTrace (\e -> (return ((),e),reverse s))


-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
throw':: Error -> StateErrorTrace a
throw' e = StateErrorTrace (const (Left e, ""))

instance MonadError StateErrorTrace where
  throw e = addTrace ("throw" ++ show e) >> throw' e

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\s -> runStateErrorTrace (lookfor' v s) s)
    where lookfor' v s = case M.lookup v s of
                          Nothing -> addTrace (v ++ " = ???") >> addTrace ("throw UndefVar " ++ v) >> throw' UndefVar
                          Just x' -> return x'

  -- Agrega una traza de actualizacion de una variable y actualiza el estado
  update v i = addTrace (unwords ["Let",v,"=",show i]) >> update' v i
    where 
      -- Actualiza el estado con el nuevo valor de la variable
      update' v i = StateErrorTrace (\s -> (Right ((), M.insert v i s), "")) 

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo
eval :: Comm -> (Either Error Env, Trace)
eval p = let
        (st, t) = runStateErrorTrace (stepCommStar p) initEnv
      in case st of
          Left err      -> (Left err, "")
          Right (_a,e') -> (Right e', reverse t)


-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= stepCommStar


-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip 
stepComm (Let v x) = evalExp x >>= update v >> return Skip
stepComm (Seq c1 c2) = stepComm c1 >> stepComm c2 
stepComm (IfThenElse b c1 c2) = do p <- evalExp b
                                   if p then stepComm c1
                                        else stepComm c2
stepComm r@(Repeat b c) = do p <- evalExp b
                             if p then stepComm c >> stepComm r 
                                  else stepComm Skip

-- Evalua a una expresion constante
evalConst :: (MonadState m, MonadError m, MonadTrace m) => a -> m a
evalConst = return


-- Toma una operacion unaria y una expresion y evalua la operacion sobre la expresion
evalOpUnaria :: (MonadState m, MonadError m, MonadTrace m, Show a, Show b) => (a->b) -> String -> Exp a -> m b
evalOpUnaria op opAsString x = do
                        x' <- evalExp x
                        let x'' = op x' in return x''


-- Evalua una operacion unaria sobre una variable entera, 
-- obteniendo la actualización de estado de la misma
evalVarOp :: (MonadState m, MonadError m, MonadTrace m) => (Int->Int) -> String -> Variable -> m Int
evalVarOp op opAsString v = do {
                          x <- lookfor v;
                          let x' = op x in do
                            ifThen2 (x/=x') (update v x');
                            return x';
                        }
  where
    ifThen2 :: Applicative f => Bool -> f () -> f ()
    ifThen2 pred expr = if pred then expr else pure ()

-- Funcion que chequea una condicion sobre 2 valores
type Check a m = a -> a -> m ()

-- Chequea que no se realize una division por 0, devolviendo un error si ocurre
checkDivByZero :: (MonadError m, MonadTrace m) => Check Int m
checkDivByZero x y = ifThen2 (y==0) (throw DivByZero)
  where
    ifThen2 :: Applicative f => Bool -> f () -> f ()
    ifThen2 pred expr = if pred then expr else pure ()


-- Toma una condicion a chequear, una operacion y dos expresiones
-- Decide en caso de pasar el chequeo, evaluar la operacion sobre las expresiones
evalDivCheck :: (MonadState m, MonadError m, MonadTrace m, Show a, Show b) => Check a m -> (a->a->b) -> String -> Exp a -> Exp a -> m b
evalDivCheck check op opAsString x y = do
                                          x1 <- evalExp x
                                          y1 <- evalExp y
                                          check x1 y1
                                          let z = (op x1 y1) in do
                                            return z

  where
    ifThen2 :: Applicative f => Bool -> f () -> f ()
    ifThen2 pred expr = if pred then expr else pure ()

-- Toma una operacion binaria y dos expresiones y evalua la operacion sobre las expresiones
evalOpBinaria :: (MonadState m, MonadError m, MonadTrace m, Show a, Show b) => (a->a->b) -> String -> Exp a -> Exp a -> m b
evalOpBinaria op opAsString x y = do
                             x1 <- evalExp x
                             y1 <- evalExp y
                             let z = (op x1 y1) in do
                              return z
                               
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
-- Int
evalExp (Const a) = evalConst a
evalExp (Var v) = evalVarOp id "" v
evalExp (VarInc v)= evalVarOp (+1) "++" v
evalExp (UMinus x) = evalOpUnaria negate "-" x
evalExp (Plus x y) = evalOpBinaria (+) "+" x y
evalExp (Minus x y) = evalOpBinaria (-) "-" x y
evalExp (Times x y) = evalOpBinaria (*) "*" x y
-- ejercicio 2
evalExp (Div x y) = evalDivCheck checkDivByZero div "/" x y

-- Bool
evalExp BTrue = evalConst True
evalExp BFalse = evalConst False
evalExp (Lt x y) = evalOpBinaria (<) "<" x y
evalExp (Gt x y) = evalOpBinaria (>) ">" x y
evalExp (And x y) = evalOpBinaria (&&) "&&" x y
evalExp (Or x y) = evalOpBinaria (||) "||" x y
evalExp (Not x) = evalOpUnaria not "!" x
evalExp (Eq x y) = evalOpBinaria (==) "==" x y
evalExp (NEq x y) = evalOpBinaria (/=) "/=" x y


