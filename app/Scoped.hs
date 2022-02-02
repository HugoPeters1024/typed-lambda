{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scoped where

import Data.GADT.Compare
import Data.Type.Equality ((:~:)(Refl))

data Nat where
    NZ :: Nat
    NS :: Nat -> Nat

data Idx n where
    Z :: Idx (NS n)
    S :: Idx n -> Idx (NS n)

deriving instance Eq (Idx n)

idxToInt :: Idx env -> Int
idxToInt Z = 0
idxToInt (S n) = 1 + idxToInt n

instance Show (Idx env) where
    show x = "x" <> show (idxToInt x)

data Term env where
    Var :: Idx n -> Term n
    Lambda :: Term (NS n) -> Term n
    App :: Term n -> Term n -> Term n

instance Show (Term n) where
    show (Var x) = show x
    show (Lambda p) = "λ" <> "." <> "(" <> show p <> ")"
    show (App f arg@(Var _)) = show f <> " " <>  show arg
    show (App f arg) = show f <> " " <> "(" <> show arg <> ")"

type Exp = Term NZ


data Env env where
    VZ :: Env '[]
    VS :: t -> Env env -> Env (t ': env)


newtype n :> n' = Weaken { (>:>) :: Idx n -> Idx n' }

infixr 9 .>
(.>) :: n2 :> n3 -> n1 :> n2 -> n1 :> n3
Weaken f .> Weaken g = Weaken (f . g)

wId :: n :> n
wId = Weaken id

wSucc :: n :> n' -> n :> NS n'
wSucc = (Weaken S .>)

wRaise :: NS n :> n' -> n :> n'
wRaise = (.> Weaken S)

wSink :: n :> n' -> NS n :> NS n'
wSink w = Weaken (\case Z -> Z ; S i -> S (w >:> i))

sinkTerm :: n :> n' -> Term n -> Term n'
sinkTerm w (Var i) = Var (w >:> i)
sinkTerm w (Lambda p) = Lambda (sinkTerm (wSink w) p)
sinkTerm w (App f arg) = App (sinkTerm w f) (sinkTerm w arg)

sinkTerm1 :: Term n -> Term (NS n)
sinkTerm1 = sinkTerm (wSucc wId)

subst :: n :> n' -> Term n' -> Term (NS n) -> Term n'
subst w = subst' weakning Z
    where weakning = Weaken $ \case
                          Z -> error "variable should have been replaced"
                          S i -> w >:> i

subst' :: n :> n' -> Idx n -> Term n' -> Term n -> Term n'
subst' w n sub (Var n') = if n == n' then sub else Var (w >:> n')
subst' w n sub (Lambda p) = Lambda (subst' (wSink w) (S n) (sinkTerm1 sub) p)
subst' w n sub (App f arg) = App (subst' w n sub f) (subst' w n sub arg)

reduce :: Term n -> Term n
reduce (Var n) = Var n
reduce (App f arg) = let rf = reduce f in case rf of
    Lambda f' -> reduce $ subst wId arg f'
    _ -> App rf (reduce arg)
reduce (Lambda p) = Lambda $ reduce p

-- | boolean logic
--
true :: Exp
true = Lambda $ Lambda $ Var (S Z)

false :: Exp
false = Lambda $ Lambda $ Var Z

ifte = Lambda $ Lambda $ Lambda $ Var (S (S Z)) `App` Var (S Z) `App` Var Z

-- | naturals

incr = Lambda $ Lambda $ Lambda $ Var (S Z) `App` (Var (S (S Z)) `App` Var (S Z) `App` Var Z)

zero = Lambda $ Lambda $ Var Z
one = reduce $ App incr zero
two = reduce $ App incr two

intToLambda :: Int -> Exp
intToLambda 0 = zero
intToLambda n = reduce $ incr `App` intToLambda (n-1)


-- | cool combinators

mockingbird :: Exp
mockingbird = Lambda $ App (Var Z) (Var Z)

ycombinator :: Exp
ycombinator = Lambda $ App
    (Lambda (App (Var (S Z)) (App (Var Z) (Var Z))))
    (Lambda (App (Var (S Z)) (App (Var Z) (Var Z))))


