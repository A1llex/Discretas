-- Martin Felipe Espinal Cruces 316155362
-- Alex Gerardo Fernandez Aguilar 314338097
-- Luis Erick Montes Garcia 419004547

data AE = Var String | Const Int | Op Rombo AE AE deriving (Eq)

instance Show AE where
 show (Var v) = show v
 show (Const c) = show c
 show (Op x y z)= show y ++ show x ++ show z 

data Rombo = Suma | Resta | Mul | Div deriving (Eq)

instance Show Rombo where
 show Resta = "-" 
 show Suma = "+"
 show Mul = "*"
 show Div = "/"

eval :: AE -> Int
eval (Const x) = x
eval (Op o (Const x) (Const y)) 
	| o == Suma = x+y
	| o == Resta = x-y
	| o == Mul = x*y
	| o == Div = div x y



posNeg :: [Int] -> [String]
posNeg xs = map (\x -> if(x >= 0) then "Positivo" else "Negativo")
 xs
