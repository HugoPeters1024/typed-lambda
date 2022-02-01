module Simplest where

data Term = Var Int | Lambda Term | App Term Term

instance Show Term where
    show (Var n) = "x" <> show n
    show (Lambda p) = "λ." <> show p
    show (App f arg) = "(" <> show f <> ") " <> show arg

-- subst variable id n, with term sub, in a term
subst' :: Int -> Term -> Term -> Term
subst' n sub (Var n') = if n == n' then sub else Var n'
subst' n sub (Lambda p) = Lambda $ subst' (n+1) sub p
subst' n sub (App f arg) = App (subst' n sub f) (subst' n sub arg)

-- Commonly used when wishing to erase a lambda
subst = subst' 0

reduce :: Term -> Term
reduce (Var n) = Var n
reduce (Lambda p) = Lambda (reduce p)
reduce (App f arg) = let rf = reduce f in case rf of
    -- After substition, more reduction options may appear
    Lambda p -> reduce $ subst arg p
    -- The function argument could not be reduced
    _        -> App rf (reduce arg)

test :: Term
test = Lambda (Lambda (Var 0)) `App` Var 1 `App` Var 2
