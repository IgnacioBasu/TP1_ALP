module Eval1
  ( eval
  , State
  )
where

import AST
import qualified Data.Map.Strict as M
import Data.Strict.Tuple

--cabal update
--cabal install strict


-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = case M.lookup v s of 
                  Just n -> n 
                  Nothing -> error ("Variable " ++ v ++ " no definida") 

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = Skip :!: s
stepComm (Let v e) s = let (val :!: s1) = evalExp e s
                       in Skip :!: update v val s1
stepComm (Seq Skip c2) s = c2 :!: s
stepComm (Seq c1 c2) s = let (c1' :!: s1) = stepComm c1 s 
                         in (Seq c1' c2) :!: s1 
stepComm (IfThenElse b c1 c2) s = let (cond :!: s1) = evalExp b s
                                  in if cond then c1 :!: s1 else c2 :!: s1
stepComm (RepeatUntil c b) s = let val = Seq c (IfThenElse b Skip (RepeatUntil c b))
                               in val :!: s
-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = n :!: s
evalExp (Var v) s = lookfor v s :!: s
evalExp (UMinus e) s =
    let (v :!: s1) = evalExp e s
    in (-v) :!: s1
evalExp (VarInc v) s =
    let val     = lookfor v s     
        newVal  = val + 1         
        newState = update v newVal s  
    in newVal :!: newState

evalExp (Plus e1 e2) s = 
                        let (v1 :!: s1) = evalExp e1 s
                            (v2 :!: s2) = evalExp e2 s1
                        in (v1 + v2) :!: s2  

-- Tenemos las dos expresiones e1 y e2, junto con el estado actual
-- Buscamos evaluar las expresiones y luego sumar sus resultados, guardando el estado final
-- En el primer let evaluamos la expresion 1 y guardamos su valor en v1, junto con s1 estado final
-- En el segundo let evaluamos la expresion 1 y guardamos su valor en v2, junto con s2 estado final
-- Finalmente devolvemos la suma de los valores y el estado final, Pair (v1+v2) estado

evalExp (Minus e1 e2) s = 
                        let (v1 :!: s1) = evalExp e1 s
                            (v2 :!: s2) = evalExp e2 s1
                        in (v1 - v2) :!: s2 

evalExp (Times e1 e2) s = 
                        let (v1 :!: s1) = evalExp e1 s
                            (v2 :!: s2) = evalExp e2 s1
                        in (v1 * v2) :!: s2 

evalExp (Div e1 e2) s = 
                        let (v1 :!: s1) = evalExp e1 s
                            (v2 :!: s2) = evalExp e2 s1
                        in (v1 `div` v2) :!: s2 

evalExp BTrue s = True :!: s

evalExp BFalse s = False :!: s

evalExp (Lt e1 e2) s = 
                        let (v1 :!: s1) = evalExp e1 s
                            (v2 :!: s2) = evalExp e2 s1
                        in (v1 < v2) :!: s2

evalExp (Gt e1 e2) s = 
                        let (v1 :!: s1) = evalExp e1 s
                            (v2 :!: s2) = evalExp e2 s1
                        in (v1 > v2) :!: s2

evalExp (And b1 b2) s = 
                        let (v1 :!: s1) = evalExp b1 s
                            (v2 :!: s2) = evalExp b2 s1
                        in (v1 && v2) :!: s2

evalExp (Or b1 b2) s = let (v1 :!: s1) = evalExp b1 s
                           (v2 :!: s2) = evalExp b2 s1
                       in (v1 || v2) :!: s2

evalExp (Not b) s = let (v :!: s1) = evalExp b s
                    in (not v) :!: s1

evalExp (Eq b1 b2) s = let (v1 :!: s1) = evalExp b1 s
                           (v2 :!: s2) = evalExp b2 s1
                       in (v1 == v2) :!: s2            

evalExp (NEq b1 b2) s = let (v1 :!: s1) = evalExp b1 s
                            (v2 :!: s2) = evalExp b2 s1
                       in (v1 /= v2) :!: s2 

-- Lo probaremos con:
-- stack exec TP1-exe -- ejemplos/sqrt.lis -e 1 (El ultimo numero marca el numero de evaluador)
-- El sqrt.lis corta cunando t >= 25, que sera cuando i = 5 (6ta iteracion)
-- El error1.lis es una division por cero
-- El error2.lis tiene una variable no definida (El evaluador muestra cual es la variable)