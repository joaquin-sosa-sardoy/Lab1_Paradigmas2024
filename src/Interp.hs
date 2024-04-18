module Interp
  ( interp,
    initial,
  )
where

import Dibujo
import FloatingPic
import Graphics.Gloss (Display (InWindow), color, display, makeColorI, pictures, translate, white, Picture)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial (Conf n dib intBas) size = display win white $ withGrid fig size
  where
    win = InWindow n (ceiling size, ceiling size) (0, 0)
    fig = interp intBas dib (0, 0) (size, 0) (0, size)
    desp = -(size / 2)
    withGrid p x = translate desp desp $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 100 100 100 100

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov p q = pictures[p,q]

-- "V" viene de import qualified Graphic.Gloss.Data.Point.Arithmetic
r45 :: FloatingPic -> FloatingPic
r45 f d w h = f(d V.+ half(w V.+ h)) (half(w V.+ h)) (half(h V.- w))

rot :: FloatingPic -> FloatingPic
rot f d w h = f(d V.+ w) h (V.negate w)

--Espejar
esp :: FloatingPic -> FloatingPic
esp f d w h = f(d V.+ w) (V.negate w) h

--Encimar
sup :: FloatingPic -> FloatingPic -> FloatingPic
sup f g d w h = pictures [f d w h , g d w h]

jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun m n f1 f2 d w h = pictures [f1 d ((m/(m+n)) V.* w) h  , f2 (d V.+ ((m/(n+m))V.* w)) ((n/(n+m)) V.* w) h] --Falta hacer

api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api m n f g d w h = pictures [f (d V.+ (n/(m+n)V.*h)) w ((m/(n+m)) V.* h) , g d w ((n/(n+m) V.* h))]

interp :: Output a -> Output (Dibujo a)  -- Traducir las funciones de dibujo.hs a las de vectores que estan arriba de esta
interp f b = foldDib f rot esp r45 api jun sup b
