module Dibujo (encimar, figura, juntar, rot45, rotar, espejar, (.-.), (///), (^^^) ) where

-- nuestro lenguaje 
data Figura = Triangulo Float Float Float | Rectangulo Float Float Float | Circulo Float Float Float deriving(Eq, Show)
at



data Dibujo a = Vacia 
                | figura a 
                | encimar (Dibujo a) (Dibujo a) 
                | apilar Float Float (Dibujo a) (Dibujo a) 
                | juntar Float Float (Dibujo a) (Dibujo a) 
                | espejar (Dibujo a)
                | rot45 (Dibujo a)
                | rotar (Dibujo a)                
                deriving (Eq, Show)

-- NO sabemos si estas irian en el data Dibujo a.
-- | (^^^) (Dibujo a) (Dibujo a)
-- | (///) (Dibujo a) (Dibujo a)
-- | (.-.) (Dibujo a) (Dibujo a)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

-- 
-- Composición n-veces de una función con sí misma. Componer 0 veces
-- es la función identidad, componer 1 vez es aplicar la función 1 vez, etc.
-- Componer negativamente es un error!

comp :: Int -> (a -> a) -> a -> a
comp 0 f x = x
comp n f x = comp (n-1) f (f(x))


-- Funciones constructoras

figura :: a -> Dibujo a
figura a (x,y,z) = undefined

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar a (a,b,c) b (d,f,g) = 

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = undefined

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = undefined

rotar :: Dibujo a -> Dibujo a
rotar [f,a,b,c] = [a,b,c]


espejar :: Dibujo a -> Dibujo a
espejar = undefined

-- Fin De Funciones Constructoras

-- Superpone un dibujo con otro. ENCIMAR
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) a b = encimar a b

--Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio. APILAR
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) a b = apilar 1 1 a b
 
-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio. JUNTAR
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) a b = juntar 1 1 a b

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 a = rotar a

r180 :: Dibujo a -> Dibujo a
r180 = comp 2 rotar a

r270 :: Dibujo a -> Dibujo a
r270 = comp 3 rotar a

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 a = (^^^)(a , (^^^)(rotar(a), (^^^)(rotar(rotar a), rotar(rotar(rotar a)))))

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto p q r s = (.-.) ((///) (p,q), (///) (r,s))

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar a = cuarteto (a, rotar (a), rotar(rotar a), (rotar (rotar a)))

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib = undefined
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change = undefined

-- Principio de recursión para Dibujos.
-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de intro a la lógica
-- foldDib aplicado a cada constructor de Dibujo debería devolver el mismo
-- dibujo

foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib = undefined


-- DE MIENTRAS COMENTO ACA Y LUEGO BORRAMOS:

-- Respuesta 1:

-- Están seraparadas las funcionalidades en los módulos indicados porque: nos permite que sea más legible y 
-- entendible de dónde a dónde cada módulo trabaja.

-- El dividir los módulos nos permite dividir un problema grande, en problemas más chicos y al ser modulos 
-- independientes se puede testear sus funcionalidades por separado. Así nos podemos asegurar que lo que venimos 
-- haciendo funciona y no nos afectará lo que se esté realizando actualmente. 

-- ## En el módulo Dibujo.hs se realiza la declaración de la sintaxis de nuestro lenguaje de figuras, definiendo 
-- los constructores del tipo Dibujo y las funciones que podremos utilizar con ellas.

-- ##Luego el módulo Interp.hs se encarga de realizar la semantica de nuestro lenguaje, dando una interpretación 
-- geometrica real utilizando vectores. 

-- ## Finalmente Main.hs es el módulo que nos permite realizar los llamados a otros módulos y configurarlos para 
-- realizar los dibujos mediante la librería Gloss.
--
-- Respuesta 2:

-- Las imágenes/figuras básicas no están incluídas en la definición del lenguaje porque cada uno puede darle la
-- interpretación que quiere. Se utiliza un tipo abstracto porque hay infinitas figuras, y dar 
-- la definición de una figura básica depende totalmente del que la quiera crear.