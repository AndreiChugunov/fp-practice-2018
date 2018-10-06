module Task1_2 where

import Todo(todo)
import Prelude hiding (gcd, pow)
-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y | x <= 0 || y <= 0 = error "Unsupported: negative numbers."
        | x == 0 && y == 0 = error "Error: x = 0; y = 0."
        | y == 0 = x
        | otherwise = gcd y (x `rem` y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = let 
                                   flSqrt = floor . sqrt . fromIntegral
                                   ceSqrt = ceiling . sqrt . fromIntegral
                                 in (flSqrt (to - 1) - ceSqrt from) >= 0 


-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year | day <= 0 || month <= 0 || year <= 0 = False
                             | not (month `elem` monthes) = False  
                             | day `elem` [1 .. days !! (fromIntegral month - 1)] = True
                             | isLeapFebruary year month && day == 29 = True
                             | otherwise = False
  where
    isLeapYear year = (year `rem` 4 == 0 && year `rem` 100 /= 0) || (year `rem` 400 == 0)
    isLeapFebruary year month = month == 2 && isLeapYear year
    days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    monthes = [1..12]

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x y | y < 0 = error "Negative pow"
        | otherwise = pow' x y x
  where
    pow' x 1 _ = x
    pow' x y z = pow' (x * z) (y - 1) z  

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x | x <= 0 = error "Number is <= 0"
          | otherwise = null [y | y <- [2..x-1], x `rem` y == 0]

-- a bit more effective implementation
isPrime' :: Integer -> Bool
isPrime' x | x <= 0 = error "Number is <= 0"
           | otherwise = checkPrime x [2 .. x-1]
  where
    checkPrime x [] = True
    checkPrime x (y:ys) = if (x `rem` y == 0) then False else checkPrime x ys

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
