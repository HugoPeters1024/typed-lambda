{-# LANGUAGE GADTs #-}
module Harder where

import Unsafe.Coerce

data Term a where
    Var :: Int -> Term a
    Lambda :: Term b -> Term (a -> b)
    App :: Term (a -> b) -> Term a -> Term b

instance Show (Term a) where
    show (Var n) = "x" <> show n
    show (Lambda p) = "λ." <> show p
    show (App f arg) = "(" <> show f <> ") " <> show arg

test = Lambda (Lambda (Var 0))

subst' :: Int -> Term a -> Term b -> Term b
-- There is no evidence that a ~ b (cause it isn't always true...)
subst' n sub (Var n') = if n == n' then unsafeCoerce sub else Var n'
subst' n sub (Lambda p) = Lambda $ subst' (n+1) sub p
subst' n sub (App f arg) = App (subst' n sub f) (subst' n sub arg)


subst = subst' 0

--subst :: forall a b. (Typeable a, Typeable b) => Int -> Prog a -> Prog b -> Prog b
--subst idx sub orginal@(Term idx_sub) = if idx == idx_sub && typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy b) then unsafeCoerce sub else orginal
--subst idx sub (Abstr t p) = Abstr t (subst idx sub p)
--subst idx sub (App f a) = App (subst idx sub f) (subst idx sub a)
--
eval :: Term a -> a
eval = eval' []
    where eval' :: [t] -> Term a -> a
          eval' env (Var n) = unsafeCoerce $ env !! n
          eval' env (Lambda p) = \arg -> eval' (unsafeCoerce arg:env) p
          eval' env (App f arg) = eval f (eval arg)





