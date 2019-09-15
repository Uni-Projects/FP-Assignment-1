-- s1024726 - Johan Urban
-- s1023775 - Paolo Scattolin
-- Assignment 1 : exercise 1.8
module Shapes
where

data Shape
  =  Circle Double            -- radius
  |  Square Double            -- length
  |  Rectangle Double Double  -- length and width
  deriving (Show)

showShape :: Shape -> String
showShape (Circle r)       =  "circle of radius " ++ show r
showShape (Square l)       =  "square of length " ++ show l
showShape (Rectangle l w)  =  "rectangle of length " ++ show l
                                ++ " and width " ++ show w

area        :: Shape -> Double
area (Circle r) = r**2 * pi
area (Square l) = l**2
area (Rectangle l w) = l*w

perimeter   :: Shape -> Double
perimeter (Circle r) = pi*r*2
perimeter (Square l) = l*4
perimeter (Rectangle l w) = l*2 + w*2

center       :: Shape -> (Double, Double)  -- x- and y-coordinates
center (Circle r) = (r, r)
center (Square l) = (l/2, l/2)
center (Rectangle l w) = (l/2, w/2)
 
boundingBox  :: Shape -> (Double, Double)  -- width and height
boundingBox (Circle r) = (2*r, 2*r)
boundingBox (Square l) = (l, l)
boundingBox (Rectangle l w) = (w, l)


