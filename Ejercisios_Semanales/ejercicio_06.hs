-- Martin Felipe Espinal Cruces 316155362
-- Alex Gerardo Fernandez Aguilar 314338097
-- Luis Erick Montes Garcia 419004547

data AE = Var String | Const Int | Op Operacion AE AE deriving (Show)

data Operacion = Suma | Resta | Mult | Div deriving (Show)

type Estado = [(String,AE)]

--solo opera con lamdas la operacuib deseada
opera :: Operacion -> (Int -> Int -> Int)
opera Suma = (\x y -> x + y)
opera Resta = (\x y -> x - y)
opera Mult = (\x y -> x * y)
opera Div = (\x y -> x `div` y)

--busca el string en el primer elelemnto de la lista de tubplas
busca :: String -> Estado -> AE
busca a ((x,y):xs)
 | (a == x) = y
 | otherwise = busca a xs   
busca a [] = error"No se encontró la variable"

--revisa los casos posibles para operar con las constantes y las variables
eval :: AE -> Estado -> Int
eval (Const x) e = x
eval (Var x) e = eval (busca x e) [] 
eval (Op o (Const x) (Const y ) ) e = opera o x y

eval (Op o (Var x) (Var y ) ) e = eval (Op o (busca x e) (busca y e) ) e
eval (Op o (Const x) (Var y ) ) e = eval (Op o (Const x) (busca y e) ) e
eval (Op o (Var y) (Const x ) ) e = eval (Op o (Const x) (busca y e) ) e

