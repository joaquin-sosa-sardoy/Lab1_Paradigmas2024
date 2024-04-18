module Dibujo (encimar, figura, juntar, rot45, apilar, rotar, espejar, (.-.), (///), (^^^), foldDib,Dibujo) where

-- nuestro lenguaje 
data Figura = Triangulo Float Float Float | Rectangulo Float Float Float | Circulo Float Float Float deriving(Eq, Show)

data Dibujo a = Figura a
 | Encimar (Dibujo a) (Dibujo a) 
 | Apilar Float Float (Dibujo a) (Dibujo a) 
 | Juntar Float Float (Dibujo a) (Dibujo a) 
 | Espejar (Dibujo a)
 | Rot45 (Dibujo a)
 | Rotar (Dibujo a) 
 deriving (Eq, Show)

--"a" toma un tipo generico
-- Despues deberiamos ver que 'a' concreto va a tomar.
-- Puede ser un entero, bool, etc.
-- Puedo asociar el True a un triangulo y False a un circulo. (o sea, solo podremos codificar 2)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

-- Composición n-veces de una función con sí misma. Componer 0 veces
-- es la función identidad, componer 1 vez es aplicar la función 1 vez, etc.
-- Componer negativamente es un error!

comp :: Int -> (a -> a) -> a -> a
comp 0 f x = x
comp n f x = comp (n-1) f (f(x))

-- Funciones constructoras

figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

-- Fin De Funciones Constructoras

-- Superpone un dibujo con otro. ENCIMAR
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar

--Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio. APILAR
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1
-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio. JUNTAR
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 a = Rotar a

r180 :: Dibujo a -> Dibujo a
r180 a = comp 2 Rotar a

r270 :: Dibujo a -> Dibujo a
r270 a = comp 3 Rotar a

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 a = encimar(encimar r1 (r90 r1)) (encimar (r90(r90 r1)) (r90(r90(r90 r1)))) 
    where
        r1 = espejar(rot45 a)
-- Esto es lo mismo que (^^^) a ((^^^)(r90 a) (r180 a)) (r270 a))

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto p q r s = (.-.) ((///) p q) ((///) r s)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar a = cuarteto a (rotar (a)) (rotar(rotar a)) (rotar(rotar (rotar a)))

-- map para nuestro lenguaje
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib g (Figura a) = g a
mapDib g (Encimar d1 d2) = Encimar (mapDib g d1) (mapDib g d2)
mapDib g (Apilar f1 f2 d1 d2) = Apilar f1 f2 (mapDib g d1) (mapDib g d2)
mapDib g (Juntar f1 f2 d1 d2) = Juntar f1 f2 (mapDib g d1) (mapDib g d2)
mapDib g (Rot45 d1) = Rot45 (mapDib g d1)
mapDib g (Rotar d1) = Rotar (mapDib g d1)
mapDib g (Espejar d1) = Espejar (mapDib g d1)
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
--change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
--change a = mapDib g a


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
foldDib fFig fRot fEsp fRot45 fAp fJun fEnc d =
    case d of 
        Figura a -> fFig a
        Rotar a -> fRot (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a)
        Espejar a -> fEsp (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a)
        Rot45 a -> fRot45 (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a)
        (Apilar f1 f2 a b) -> fAp f1 f2 (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a) (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc b)
        (Juntar f1 f2 a b) -> fJun f1 f2 (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a) (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc b)
        (Encimar a b) -> fEnc (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a) (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc b)


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