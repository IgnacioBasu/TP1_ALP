

-- Integrantes: Basualdo Ignacio, Capezio Lautaro, Duarte Luciano

module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name =  Global  String
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = EmptyT 
            | FunT Type Type
            -- Extension naturales ejercicio 4
            | NatT
            | ListT
            deriving (Show, Eq)
  
  -- Términos con nombres
  data LamTerm  =  LVar String
                |  LAbs String Type LamTerm
                |  LApp LamTerm LamTerm
                -- Extension let ejercicio 3
                |  LLet String LamTerm LamTerm
                -- Extension naturales ejercicio 4
                |  LZero
                |  LSuc LamTerm
                |  LRec LamTerm LamTerm LamTerm
                -- Extension listas de naturales ejercicio 6
                |  LNil
                |  LCons LamTerm LamTerm
                deriving (Show, Eq)


  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name 
             | Term :@: Term
             | Lam Type Term
             -- Extension let ejercicio 3
             | Let Term Term
             -- Extension naturales ejercicio 4
             | Zero
             | Suc Term
             | Rec Term Term Term
             -- Extension listas de naturales ejercicio 6
             | Nil
             | Cons Term Term
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             -- Extension naturales ejercicio 4
             | VNum NumVal
             | VList ListVal
             
           deriving (Show, Eq)

  -- Valores Numericos
  data NumVal = NZero | NSuc NumVal deriving (Show, Eq)

  -- Listas de números
  data ListVal = VNil | VCons NumVal ListVal deriving (Show, Eq)
   
  -- Contextos del tipado
  type Context = [Type]
