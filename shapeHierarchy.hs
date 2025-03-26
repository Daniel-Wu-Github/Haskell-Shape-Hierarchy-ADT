{-Enter your name: Daniel Wu
  Enter the date: 2/17/2025
  (by entering your name and date you certify this is your
  own work and not that of any other person or service.
  Comment your functions for clarity.-}

--ToDo  Define an ADT (Abstract Data Type) for Shape.
--Implement polymorphism using Haskell's type classes.
--Implement methods for calculating the area and perimeter of various shapes.
--Use recursion and higher-order functions to manage a collection of shapes.
--add functionality to calculate volume

{-data Shape = Circle Double
            | Rectangle Double Double
            | Triangle Double Double Double
            | Pentagon Double Double Double Double Double
            | Hexagon Double Double Double Double Double Double
            deriving (Show, Read)

--function to validate a triangle
isValidTriangle :: Double -> Double -> Double -> Bool
isValidTriangle a b c = (a + b > c && b + c > a && a + c > b)
area :: Shape -> Maybe Double -- maybe we will get good data
area (Circle r) = Just (pi * r * r) -- Just keyword means we have a value
area (Rectangle w h) = Just (w * h)
area (Triangle a b c)
    | isValidTriangle a b c = Just (let s = (a + b + c) / 2 in sqrt (s * (s - a) * (s - b) * (s - c)))
    | otherwise = Nothing -- nothing keyword will return nothing in the case of an invalid triangle
area (Pentagon a b c d e) = Just ((1/4) * sqrt(5 * (5 + 2 * sqrt(5))) * (a^2))
area (Hexagon a b c d e f) = Just (3 * sqrt(3) * (a^2)) -- area of a hexagon

perimeter :: Shape -> Maybe Double
perimeter (Circle r) = Just (2 * pi * r)
perimeter (Rectangle w h) = Just (2 * w + 2 * h)
perimeter (Triangle a b c) = Just (a + b + c)
perimeter (Pentagon a b c d e) = Just (a + b + c + d + e)
perimeter (Hexagon a b c d e f) = Just (a + b + c + d + e + f)

--data for 3D objects
data Prism = Sphere Double
            | RectangularPrism Double Double Double
            | Pyramid Double Double Double
            | PentagonalPrism Double Double
            | HexagonalPrism Double Double
            deriving (Show, Read)

volume :: Prism -> Maybe Double
volume (Sphere r) = Just ((4/3) * pi * r^3)
volume (RectangularPrism w h l) = Just (w * h * l)
volume (Pyramid bw bh h) = Just ((1/3) * bw * bh * h)
volume (PentagonalPrism s h) = Just ((3/2) * sqrt(3) * s^2 * h)
volume (HexagonalPrism s h) = Just ((1/4) * sqrt(5*(5 + 2 * sqrt(5))) * s^2 * h)

--function to calculate the total area of all shapes
totalArea :: [Shape] -> Double
totalArea shapes = sum (map (maybe 0 id . area) shapes)

--function to calculate the total perimeter of all shapes
totalPerimeter :: [Shape] -> Double
totalPerimeter shapes = sum (map (maybe 0 id . perimeter) shapes)

--function to calculate the total volume of all shapes
totalVolume :: [Prism] -> Double
totalVolume prisms = sum (map (maybe 0 id . volume) prisms)

--fucntion to format result to a string
formatResult :: Maybe Double -> String
formatResult = maybe "Invalid Shape" show-}

isValidTriangle :: Double -> Double -> Double -> Bool -- function to validate a triangle
isValidTriangle a b c = (a + b > c && b + c > a && a + c > b)

-- Polymorphism using type class
class Shape a where
  area :: a -> Maybe Double
  perimeter :: a -> Maybe Double

-- Individual shapes as instances of the Shape class
data Circle = Circle Double deriving (Show, Read) -- data type for a circle
instance Shape Circle where
  area (Circle r) = Just (pi * r * r)
  perimeter (Circle r) = Just (2 * pi * r)

data Rectangle = Rectangle Double Double deriving (Show, Read) -- data type for a rectangle
instance Shape Rectangle where
  area (Rectangle w h) = Just (w * h)
  perimeter (Rectangle w h) = Just (2 * w + 2 * h)

data Triangle = Triangle Double Double Double deriving (Show, Read) -- data type for a triangle
instance Shape Triangle where
  area (Triangle a b c)
    | isValidTriangle a b c = Just (let s = (a + b + c) / 2 in sqrt (s * (s - a) * (s - b) * (s - c)))
    | otherwise = Nothing
  perimeter (Triangle a b c) = Just (a + b + c)

data Pentagon = Pentagon Double Double Double Double Double deriving (Show, Read) -- data type for a pentagon
instance Shape Pentagon where
  area (Pentagon a _ _ _ _) = Just ((1/4) * sqrt(5 * (5 + 2 * sqrt(5))) * (a^2))
  perimeter (Pentagon a b c d e) = Just (a + b + c + d + e)

data Hexagon = Hexagon Double Double Double Double Double Double deriving (Show, Read) -- data type for a hexagon
instance Shape Hexagon where
  area (Hexagon a _ _ _ _ _) = Just (3 * sqrt(3) * (a^2))
  perimeter (Hexagon a b c d e f) = Just (a + b + c + d + e + f)

totalArea :: (Shape a) => [a] -> Double -- function to calculate the total area of the shape given, works for all shapes
totalArea shapes = sum (map (maybe 0 id . area) shapes)

totalPerimeter :: (Shape a) => [a] -> Double -- function to calculate the total perimeter of the shape given, works for all shapes
totalPerimeter shapes = sum (map (maybe 0 id . perimeter) shapes)

class Prism a where -- type class for 3D objects
  volume :: a -> Maybe Double

data Sphere = Sphere Double deriving (Show, Read)
instance Prism Sphere where
  volume (Sphere r) = Just ((4/3) * pi * r^3) -- volume of a sphere

data RectangularPrism = RectangularPrism Double Double Double deriving (Show, Read)
instance Prism RectangularPrism where
  volume (RectangularPrism w h l) = Just (w * h * l) -- volume of a rectangular prism

data Pyramid = Pyramid Double Double Double deriving (Show, Read)
instance Prism Pyramid where
  volume (Pyramid bw bh h) = Just ((1/3) * bw * bh * h) -- volume of a pyramid

data PentagonalPrism = PentagonalPrism Double Double deriving (Show, Read)
instance Prism PentagonalPrism where
  volume (PentagonalPrism s h) = Just ((3/2) * sqrt(3) * s^2 * h) -- volume of a pentagonal prism

data HexagonalPrism = HexagonalPrism Double Double deriving (Show, Read)
instance Prism HexagonalPrism where
  volume (HexagonalPrism s h) = Just ((1/4) * sqrt(5*(5 + 2 * sqrt(5))) * s^2 * h) -- volume of a hexagonal prism

totalVolume :: (Prism a) => [a] -> Double -- function to calculate the total volume of the prism given, works for all prisms
totalVolume prisms = sum (map (maybe 0 id . volume) prisms)


formatResult :: Maybe Double -> String -- function to format the result to a string
formatResult = maybe "Invalid Shape" show

-- Create a simple main program
main :: IO ()
main = do
    -- Separated the list of shapes into individual lists, this allows for the calculation of multiple shapes of the same type
    let circles = [Circle 10]
    let rectangles = [Rectangle 5 8]
    let triangles = [Triangle 3 4 5]
    let pentagons = [Pentagon 5 5 5 5 5]
    let hexagons = [Hexagon 6 6 6 6 6 6]

    -- same applied for the prisms
    let spheres = [Sphere 5]
    let rectangularPrisms = [RectangularPrism 5 6 7]
    let pyramids = [Pyramid 5 6 8]
    let pentagonalPrisms = [PentagonalPrism 5 10]
    let hexagonalPrisms = [HexagonalPrism 10 5]
    
    -- Print out the area and perimeter of each shape
    putStrLn "Shape Areas:"
    mapM_ (\x -> putStrLn $ "Area: " ++ formatResult (area x)) circles
    mapM_ (\x -> putStrLn $ "Area: " ++ formatResult (area x)) rectangles
    mapM_ (\x -> putStrLn $ "Area: " ++ formatResult (area x)) triangles
    mapM_ (\x -> putStrLn $ "Area: " ++ formatResult (area x)) pentagons
    mapM_ (\x -> putStrLn $ "Area: " ++ formatResult (area x)) hexagons

    putStrLn "\nShape Perimeters:"
    mapM_ (\x -> putStrLn $ "Perimeter: " ++ formatResult (perimeter x)) circles
    mapM_ (\x -> putStrLn $ "Perimeter: " ++ formatResult (perimeter x)) rectangles
    mapM_ (\x -> putStrLn $ "Perimeter: " ++ formatResult (perimeter x)) triangles
    mapM_ (\x -> putStrLn $ "Perimeter: " ++ formatResult (perimeter x)) pentagons
    mapM_ (\x -> putStrLn $ "Perimeter: " ++ formatResult (perimeter x)) hexagons

    putStrLn "\nPrism Volumes:"
    mapM_ (\x -> putStrLn $ "Volume: " ++ formatResult (volume x)) spheres
    mapM_ (\x -> putStrLn $ "Volume: " ++ formatResult (volume x)) rectangularPrisms
    mapM_ (\x -> putStrLn $ "Volume: " ++ formatResult (volume x)) pyramids
    mapM_ (\x -> putStrLn $ "Volume: " ++ formatResult (volume x)) pentagonalPrisms
    mapM_ (\x -> putStrLn $ "Volume: " ++ formatResult (volume x)) hexagonalPrisms
    
    -- Print out the total area and perimeter of all shapes
    putStrLn "\nTotal Area of All Shapes:"
    print (totalArea circles + totalArea rectangles + totalArea triangles + totalArea pentagons + totalArea hexagons)
    
    putStrLn "Total Perimeter of All Shapes:"
    print (totalPerimeter circles + totalPerimeter rectangles + totalPerimeter triangles + totalPerimeter pentagons + totalPerimeter hexagons)

    putStrLn "Total Volume of All Prisms:"
    print (totalVolume spheres + totalVolume rectangularPrisms + totalVolume pyramids + totalVolume pentagonalPrisms + totalVolume hexagonalPrisms)