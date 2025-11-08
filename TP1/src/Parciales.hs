import Parsing
import Control.Monad
import Control.Applicative

--EJERCICIO:
-- Parsear cadenas del tipo "(N 2 (N 1 (N 5) (N 6)))" a su correspondiente representacion en el tipo de dato Gtree


data Gtree a = N a [Gtree a] deriving Show

tree :: Parser (Gtree Int)
tree = do   { symbol '('
            ; symbol 'N'
            ; x <- int
            ; xs <- many tree
            ; symbol ')'
            ; return (N x xs)}


--Ejercicio 2:
-- diseñe un parser que pueda parsear cadenas de parentesis () y que lo tranforme en una [[Int]] tal que 
-- cada parentesis se vea reflejado con el numero del nivel de parentesis el cual sea:
--EJEMPLO:
-- "(())" ---> [[1,2,1]]
-- "(()) ,()" --->[[1,2,2,1][1,1]]
-- "()(()())" ---> [[1,1,1,2,2,2,2,1]]

-- ...existing code...
-- parser que retorna una lista de bloques ([[Int]]), cada bloque corresponde a
-- paréntesis adyacentes (sin coma entre ellos); los bloques están separados por ','
paren :: Parser [[Int]]
paren = sepBy1 (concat <$> many1 (group 0)) (symbol ',')

-- parser de un solo grupo de paréntesis, devolviendo la secuencia de niveles
group :: Int -> Parser [Int]
group n = do
    symbol '('
    children <- many (group (n+1))
    symbol ')'
    let lvl = n + 1
    return (lvl : concat children ++ [lvl])

-- auxiliares
many1 :: Parser a -> Parser [a]
many1 p = do { x <- p; xs <- many p; return (x:xs) }

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do { x <- p; xs <- many (sep >> p); return (x:xs) }
-- ...existing code...