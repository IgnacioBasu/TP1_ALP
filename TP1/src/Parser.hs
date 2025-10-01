module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST
import Data.Bool (bool)

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until", "case"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "++"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        ]
    }
  )

identifierT   = identifier lis      -- variables/identificadores
reservedT     = reserved lis        -- palabras clave
reservedOpT   = reservedOp lis      -- operadores
parensT       = parens lis          -- ( expr )
bracesT       = braces lis          -- { stmts }
semiT         = semi lis            -- ;
commaT        = comma lis           -- ,
naturalT      = natural lis         -- números naturales
integerT      = integer lis         -- enteros con signo
whiteSpaceT   = whiteSpace lis      -- ignora espacios/comentarios
symbolT       = symbol lis          -- símbolos como "="



-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

mulOp :: Parser (Exp Int -> Exp Int -> Exp Int)
mulOp = do
    reservedOpT "*"
    return Times
  <|> do
    reservedOpT "/"
    return Div

addOp :: Parser (Exp Int -> Exp Int -> Exp Int)
addOp = do
    reservedOpT "+"
    return Plus
  <|> do
    reservedOpT "-"
    return Minus
  
varIncOp :: Parser (Exp Int)
varIncOp = do
    v <- identifierT
    reservedOpT "++"
    return (VarInc v)

uMinusOp :: Parser (Exp Int)
uMinusOp = do
    reservedOpT "-"
    n <- parseFactor
    return (UMinus n)

parseVar :: Parser (Exp Int)
parseVar = do
    v <- identifierT
    return (Var v)

parseConst :: Parser (Exp Int)
parseConst = do
    n <- integerT
    return (Const (fromInteger n))

parseFactor :: Parser (Exp Int)
parseFactor = try varIncOp
          <|> try uMinusOp
          <|> try parseConst
          <|> try parseVar
          <|> try (do   
                     expr <- parensT intexp
                     return expr
                  ) 

parseTerm :: Parser (Exp Int)
parseTerm = parseFactor `chainl1` mulOp

intexp :: Parser (Exp Int)
intexp = parseTerm `chainl1` addOp

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
boolOp = (do
            reservedOpT "&&"
            return And
         )
     <|> (do
            reservedOpT "||"
            return Or
         )

compOp :: Parser (Exp Int -> Exp Int -> Exp Bool)
compOp = (do
            reservedOpT "=="
            return Eq
         )
     <|> (do
            reservedOpT "!="
            return NEq
         )
     <|> (do
            reservedOpT "<"
            return Lt
         )
     <|> (do
            reservedOpT ">"
            return Gt
         )
         

boolexp :: Parser (Exp Bool)
boolexp = boolterm `chainl1` boolOp

boolterm :: Parser (Exp Bool)
boolterm =
        parensT boolexp
    <|> (do
            reservedOpT "!"
            b <- boolexp
            return (Not b)
        )
    <|> (do
            reservedT "true"
            return BTrue
        )
    <|> (do
            reservedT "false"
            return BFalse
        )
    <|> (do
          e0 <- intexp
          op <- compOp
          e1 <- intexp
          return (op e0 e1)
        )



-----------------------------------
--- Parser de comandos
-----------------------------------
seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do
    reservedOpT ";"
    return Seq

comm :: Parser Comm
comm = commAux `chainl1` seqOp

ifParser :: Parser Comm
ifParser = do
    reservedT "if"
    b <- boolexp
    c1 <- bracesT comm
    
    ((do
        reservedT "else"
        c2 <- bracesT comm
        return (IfThenElse b c1 c2)
        )
        <|> (return (IfThenElse b c1 Skip))
        )
    



commAux :: Parser Comm
commAux =
    (do -- Parser para 'Let'
        v <- identifierT
        reservedOpT "="
        e <- intexp
        return (Let v e)
    )
    <|>
    (do -- Parser para 'Skip'
        reservedT "skip"
        return Skip
    )
    <|>
    ifParser
    <|>
    (do
        reservedT "repeat"
        c <- bracesT comm
        reservedT "until"
        b <- boolexp
        return (RepeatUntil c b)
    )
    <|>
    parseCase

----------------------------------------------
--- INICIO: Parser de CASE (Falta testear) :)
----------------------------------------------
caseBranch :: Parser (Exp Bool, Comm)
caseBranch = do
    b <- boolexp
    reservedOpT ":"
    c <- bracesT comm
    return (b, c)

parseCaseAux :: [(Exp Bool, Comm)] -> Comm
parseCaseAux [] = Skip
parseCaseAux ((b, c):bcs) = IfThenElse b c (parseCaseAux bcs)

parseCase :: Parser Comm
parseCase = do
    reservedT "case"
    branches <- bracesT (many caseBranch)
    return (parseCaseAux branches)
----------------------------------------------
--- FIN: Parser de CASE (Falta testear) :)
----------------------------------------------

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
