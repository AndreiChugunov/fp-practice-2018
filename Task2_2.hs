module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)
import Data.List (genericTake, genericDrop)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f a = case f a of
               Just (x, b') -> x:(unfoldr f b')
               Nothing      -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x s -> (f x):s) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldr (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x s -> case x of
                    Just y  -> y:s
                    Nothing -> s
                  ) []

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal lst = snd $ foldr (\x (i, s) -> (i - 1, x !! i : s)) (length lst - 1, []) lst

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = foldr (\x s -> if pred x then s else x:s) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e = foldr (\x s -> s || (x == e)) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step | to >= from = makeRange (>= to)
                     | otherwise = makeRange (<= to)
  where makeRange pred = unfoldr (\b -> if pred b
                                   then Nothing
                                   else Just (b, b+step)) from
-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append l1 l2 = foldr (\x s -> x:s) l2 l1

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\b -> if null b
                              then Nothing
                              else Just $ splitAt' n b) lst

splitAt' :: Integer -> [a] -> ([a],[a])
splitAt' n xs = (genericTake n xs, genericDrop n xs)
  --unfoldr (Just . splitAt n) lst
  --foldr (\x s -> splitAt n  ) (splitAt lst n)
