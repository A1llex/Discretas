module Prop where 
 
 import Data.List

--DEFINICIONES 

-- Tipo de dato para representar las expresiones de la lógica proposicional
 data Prop = Verdadero
           | Falso 
           | Var String
           | Neg Prop
           | Conj Prop Prop
           | Disy Prop Prop
           | Impl Prop Prop
           | Syss Prop Prop
           deriving (Eq,Ord)

-- Sinónimo para representar el estado
 type Estado = [(String, Prop)]

--Instancia de Show para el tipo Prop, para que sea legible lo que se imprime en consola
-- NO DEFINE COMPORTAMIENTO
 instance Show Prop where 
  show Verdadero = "V" -- V
  show Falso = "F" -- F
  show (Var x) = x -- P 
  show (Neg p) = "¬ " ++ show p -- ¬ P
  show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)


-- EQUIVALENCIAS LÓGICAS 

 -- Ejercicio 1.1
 eliminacion :: Prop -> Prop
 eliminacion (Impl (Var x) (Var y)) = (Disy (Neg(Var x)) (Var y))
 eliminacion (Syss (Var x) (Var y)) = (Conj (Disy (Neg (Var x)) (Var y)) (Disy (Var x) (Neg (Var y))))

 -- Ejercicio 1.2
 deMorgan :: Prop -> Prop
 -- Aquí va tu código
 deMorgan = error "Función no definida"
 

-- EVALUACIÓN Y ANÁLISIS SINTÁCTICO DE EXPRESIONES

 -- Ejercicio 2.1 
 interp :: Prop -> Estado -> Bool
 -- Aquí va tu código
 interp = error "Función no definida"

  -- Ejercicio 2.2
 truthTable :: Prop -> String
  -- Aquí va tu código
 truthTable = error "Función no definida"

 -- Ejercicio 2.3  
 correcto :: [Prop] -> Prop -> Bool 
  -- Aquí va tu código
 correcto = error "Función no definida"

