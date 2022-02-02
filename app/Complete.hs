{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module Complete where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import Data.GADT.Compare
import Data.Type.Equality ((:~:)(Refl))

-- | Typed De Bruijn index
data Idx env t where
    Z :: Idx (t ': env) t
    S :: Idx env t -> Idx (s ': env) t

idxToInt :: Idx env t -> Int
idxToInt Z = 0
idxToInt (S i) = 1 + idxToInt i

instance Show (Idx env t) where
    show = ("x"<>) . show . idxToInt

instance GEq (Idx env) where
    geq Z Z = Just Refl
    geq (S i) (S i') = geq i i'
    geq _ _ = Nothing

data Term env a where
    Var :: Idx env a -> Term env a
    Lambda :: Term (a ': env) b -> Term env (a -> b)
    App :: Term env (a -> b) -> Term env a -> Term env b

type Exp = Term '[]

instance Show (Term env a) where
    show (Var x) = show x
    show (Lambda p) = "λ" <> "." <> "(" <> show p <> ")"
    show (App f arg@(Var _)) = show f <> " " <>  show arg
    show (App f arg) = show f <> " " <> "(" <> show arg <> ")"

data Env env where
    VZ :: Env '[]
    VS :: t -> Env env -> Env (t ': env)


envLookup :: Env env -> Idx env t -> t
envLookup (VS x _) Z = x
envLookup (VS _ env) (S i) = envLookup env i

eval :: Exp a -> a
eval = eval' VZ
    where eval' :: Env env -> Term env a -> a
          eval' env (Var i) = envLookup env i
          eval' env (Lambda e) = \arg -> eval' (VS arg env) e
          eval' env (App f e) = eval' env f (eval' env e)



newtype env :> env' = Weaken { (>:>) :: forall t'. Idx env t' -> Idx env' t' }

infixr 9 .>
(.>) :: env2 :> env3 -> env1 :> env2 -> env1 :> env3
Weaken f .> Weaken g = Weaken (f . g)

wId :: env :> env
wId = Weaken id

wSucc :: env :> env' -> env :> (t : env')
wSucc = (Weaken S .>)

wRaise :: (t' : env) :> env' -> env :> env'
wRaise = (.> Weaken S)

wSink :: env :> env' -> (t ': env) :> (t ': env')
wSink w = Weaken (\case Z -> Z ; S i -> S (w >:> i))

sinkTerm :: env :> env' -> Term env t -> Term env' t
sinkTerm w (Var i) = Var (w >:> i)
sinkTerm w (Lambda e) = Lambda (sinkTerm (wSink w) e)
sinkTerm w (App f arg) = App (sinkTerm w f) (sinkTerm w arg)

sinkTerm1 :: Term env t -> Term (a ': env) t
sinkTerm1 = sinkTerm (wSucc wId)

subst :: env :> env' -> Term env' u -> Term (u ': env) t -> Term env' t
subst w = subst' weakening Z
    where weakening = Weaken $ \case
                            Z -> error "variable should have been replaced"
                            S i -> w >:> i


subst' :: env :> env' -> Idx env a -> Term env' a -> Term env t -> Term env' t
subst' w n sub (Var n')
    | Just Refl <- geq n n' = sub
    | otherwise = Var (w >:> n')
subst' w n sub (Lambda e) = Lambda (subst' (wSink w) (S n) (sinkTerm1 sub) e)
subst' w n sub (App f arg) = App (subst' w n sub f) (subst' w n sub arg)

reduce :: Term env a -> Term env a
reduce (Var n) = Var n
reduce (App f arg) = let rf = reduce f in case rf of
    Lambda f' -> reduce $ subst wId arg f'
    _ -> App rf (reduce arg)
reduce (Lambda e) = Lambda $ reduce e


-------- Examples

chain :: Exp (a -> b) -> Exp (b -> c) -> Exp (a -> c)
chain lhs rhs = Lambda $ App (sinkTerm1 rhs) (App (sinkTerm1 lhs) (Var Z))

discardfirst :: Exp (a -> b -> b)
discardfirst = Lambda $ Lambda $ Var Z

-- | boolean logic

true = Lambda $ Lambda $ Var (S Z)
false = Lambda $ Lambda $ Var Z
ifte = Lambda $ Lambda $ Lambda $ Var (S (S Z)) `App` Var (S Z) `App` Var Z

-- naturals
incr = Lambda $ Lambda $ Lambda $ Var (S Z) `App` (Var (S (S Z)) `App` Var (S Z) `App` Var Z)

zero = Lambda $ Lambda $ Var Z
one = reduce $ App incr zero
two = reduce $ App incr two

intToLambda :: Int -> Exp ((a -> a) -> a -> a)
intToLambda 0 = zero
intToLambda n = reduce $ incr `App` intToLambda (n-1)

-- | Impossible :(
-- mockingbird = Lambda $ App (Var Z) (Var Z)

-- | Also not possible :(
--ycombinator :: Exp ((b -> b) -> b)
--ycombinator = Lambda $ App
--    (Lambda (App (Var (S Z)) (App (Var Z) (Var Z))))
--    (Lambda (App (Var (S Z)) (App (Var Z) (Var Z))))


