--Luis Erick Montes Garcia 
--Alex Gerardo Fernández Aguilar

data Figura = Circulo Float | Cuadrado Float | Rectangulo Float Float | Triangulo Float Float  deriving (Show,Eq)

area :: Figura -> Float
area (Circulo r) = pi * (r * r)
area (Cuadrado l) = l * l
area (Rectangulo  b h) = b * h
area (Triangulo b h) =  b * h

loki :: Int -> Bool -> String
loki a True 
    |(a<25)&&(a>15) = "Sale a jugar"
    |otherwise = "No sale a jugar"
loki a False
    |(a<30)&&(a>20)="Sale a jugar"
    |otherwise = "No Sale a jugar"
    


suma1 :: [Int] -> [Int]
suma1 x = map(+1)(x) 

areaHeron :: Float -> Float -> Float -> Float
areaHeron x y z = sqrt ( s * (s-x) * (s-y) * (s-z)  )
    where s = (x + y + z) / 2 

