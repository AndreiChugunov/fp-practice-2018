module Task1_1 where

import Todo(todo)

data BinOp = Plus | Minus | Mult 
  deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, binOp :: BinOp, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l Plus r
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l Minus r
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l Mult r

infixl 6 |+| 
infixl 6 |-| 
infixl 7 |*| 

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
                                              v@(Variable s)    -> if (s == varName) then replacement else v
                                              BinaryTerm l op r -> BinaryTerm (replaceVar varName replacement l) op (replaceVar varName replacement r)
                                              _                 -> expression  

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm l op r) = evaluate' (evaluate l) op (evaluate r)
   where 
    evaluate' :: Term -> BinOp -> Term -> Term
    evaluate' (IntConstant c1) Plus  (IntConstant c2) = IntConstant $ c1 + c2
    evaluate' (IntConstant 0)  Plus  t                = t
    evaluate' t                Plus  (IntConstant 0)  = t                
    evaluate' (IntConstant c1) Minus (IntConstant c2) = IntConstant $ c1 - c2
    evaluate' t                Minus (IntConstant 0)  = t
    evaluate' (IntConstant c1) Mult  (IntConstant c2) = IntConstant $ c1 * c2
    evaluate' (IntConstant 0)  Mult  _                = IntConstant 0  
    evaluate' (IntConstant 1)  Mult  t                = t
    evaluate' _                Mult  (IntConstant 0)  = IntConstant 0  
    evaluate' t                Mult  (IntConstant 1)  = t                 
    evaluate' l                op    r                = BinaryTerm  l op r 

evaluate term = term