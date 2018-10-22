module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)
import Control.Monad.Trans.State

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree
               | Leaf (Integer, v)
               | Fork (TreeMap v) (Integer, v) (TreeMap v)
  deriving (Eq, Show)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree k = False
contains (Leaf x) k = fst x == k
contains (Fork l v r) k | fst v == k = True
                        | fst v < k = contains r k
                        | otherwise = contains l k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k EmptyTree = error "No such key"
lookup k (Leaf x)     | fst x == k = snd x
                      | otherwise = error "No such key"
lookup k (Fork l v r) | fst v == k = snd v
                      | fst v < k = lookup k r
                      | otherwise = lookup k l

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert p EmptyTree = Leaf p
insert p@(k, v) l@(Leaf x) | k == fst x = l
                           | k > fst x = Fork EmptyTree x (Leaf p)
                           | otherwise = Fork (Leaf p) x EmptyTree
insert p@(k, v) f@(Fork l x r) | k == fst x = f
                               | k > fst x = Fork l x (insert p r)
                               | otherwise = Fork (insert p l) x r

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i EmptyTree = error "EmptyTree"
remove i l@(Leaf x)   | fst x == i = EmptyTree
                      | otherwise = l
remove i (Fork l x r) | fst x == i = concatTrees l r
                      | i > fst x = Fork l x (remove i r)
                      | otherwise = Fork (remove i l) x r
  where
     concatTrees EmptyTree t = t
     concatTrees (Leaf x) t = Fork EmptyTree x t
     concatTrees (Fork l x r) t = Fork l x (concatTrees r t)


--Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i EmptyTree = error "EmptyTree"
nearestLE i (Leaf x) = error "Only one element in a tree"
nearestLE i f@(Fork _ x _) = let (s1, s2) = execState (nearestLE' i f) (x, x)
                             in if (fst s1 /= i)
                                then error "There is no such a key"
                                else if (fst s1 < fst s2)
                                     then error $ "This is first key. "
                                               <> "It doesn't have pred"
                                     else s2
  where
    nearestLE' :: Integer -> TreeMap v -> State ((Integer, v), (Integer, v)) ()
    nearestLE' i EmptyTree = return ()
    nearestLE' i (Leaf x) = do
      modify (\(s1, s2) -> if (fst x <= i) then (x, s1) else (s1, s2))
    nearestLE' i (Fork l x r) = do
      left <- nearestLE' i l
      if (fst x >= i)
      then do
        modify (\(s1, s2) -> if (fst x == i) then (x, s1) else (s1, s2))
      else do
            modify (\(s1, s2) -> (x, s1))
            right <- nearestLE' i r
            return ()


-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList [] = EmptyTree
treeFromList (x:xs) = insert x (treeFromList xs)

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Leaf x) = [x]
listFromTree (Fork l x r) = [x]
                         <> listFromTree l
                         <> listFromTree r

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i EmptyTree = error "EmptyTree"
kMean i (Leaf x) | i == 0 = x
                 | otherwise = error "Bad index"

kMean i f@(Fork l x r) | i < 0 = error "Bad index"
                       | otherwise = let (ind, st) = (execState (kMean' f) (i, x))
                                     in if (ind == -1) then st
                                        else error "Index too large"
  where
    kMean' :: TreeMap v -> State (Integer, (Integer, v)) ()
    kMean' EmptyTree = return ()
    kMean' (Leaf x) = do
      modify (\(i, s) -> if (i == 0) then (i-1, x) else (i-1, s))
    kMean' (Fork l x r) = do
      left <- kMean' l
      (i, s) <- get
      if (i == -1)
      then return ()
      else do
        put (i-1, x)
        if (i == 0)
        then return ()
        else do
          right <- kMean' r
          return ()
