{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Infer where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    foldM,
    replicateM,
    runExceptT,
  )
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    foldM,
    replicateM,
  )
import Data.Foldable (foldr)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Syntax
import Type
import Prelude hiding (foldr)

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)
  deriving (Monoid, Semigroup)

newtype Unique = Unique {count :: Int}

type Infer = ExceptT TypeError (State Unique)

type Subst = Map.Map TVar Type

data TypeError where
  UnificationFail :: Type -> Type -> TypeError
  InfiniteType :: TVar -> Type -> TypeError
  UnboundVariable :: String -> TypeError

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where
    sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique {count = 0}

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Var -> Maybe Type.Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substitutable Type where
  apply :: Subst -> Type -> Type
  apply _ (TCon a) = TCon a
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArrow` t2) = apply s t1 `TArrow` apply s t2
  apply s (TArray a) = TArray (apply s a)

  ftv TCon {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArrow` t2) = ftv t1 `Set.union` ftv t2
  ftv (TArray a) = ftv a -- Free variables of an array include the free variables of its element type

instance Substitutable Scheme where
  apply :: Subst -> Scheme -> Scheme
  apply s (Forall as t) = Forall as $ apply s' t
    where
      s' = foldr Map.delete s as
  ftv :: Scheme -> Set.Set TVar
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance (Substitutable a) => Substitutable [a] where
  apply :: (Substitutable a) => Subst -> [a] -> [a]
  apply = fmap . apply
  ftv :: (Substitutable a) => [a] -> Set.Set TVar
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply :: Subst -> TypeEnv -> TypeEnv
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv :: TypeEnv -> Set.Set TVar
  ftv (TypeEnv env) = ftv $ Map.elems env

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify :: Type -> Type -> Infer Subst
unify (l `TArrow` r) (l' `TArrow` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return nullSubst
unify (TArray a) (TArray b) = unify a b
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Infer Subst
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ Map.singleton a t

occursCheck :: (Substitutable a) => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Binop -> Type
ops Add = typeInt `TArrow` typeInt `TArrow` typeInt
ops Mul = typeInt `TArrow` typeInt `TArrow` typeInt
ops Sub = typeInt `TArrow` typeInt `TArrow` typeInt
ops Eql = typeInt `TArrow` typeInt `TArrow` typeBool

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s -> do
      t <- instantiate s
      return (nullSubst, t)

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of
  Var x -> lookupEnv env x
  -- TODO-2: Handle the different pattern values of `x`
  --         Each has its own implications for typing
  --
  Lam (PVar x) e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArrow` t1)
  Lam (PCons x y) e -> do
    tv <- fresh
    let env' = env `extend` (y, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArrow` t1)
  Lam (PLit x) e -> do
    tv <- fresh
    -- let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env e -- should be env'
    return (s1, apply s1 tv `TArrow` t1)
  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) (TArrow t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t' = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)
  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TArrow` tv `TArrow` tv `TArrow` tv)
  Fix e1 -> do
    tv <- fresh
    inferPrim env [e1] ((tv `TArrow` tv) `TArrow` tv)
  Op Cons e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2
    case t2 of
      TArray elemType -> do
        s3 <- unify t1 elemType
        return (s3 `compose` s2 `compose` s1, TArray elemType)
      _ -> throwError $ UnificationFail t1 (TArray t2)
  Op Concat e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2
    case (t1, t2) of
      (TArray elemType1, TArray elemType2) -> do
        s3 <- unify elemType1 elemType2
        return (s3 `compose` s2 `compose` s1, TArray elemType1)
      _ -> throwError $ UnificationFail t1 (TArray t2)
  Op op e1 e2 -> do
    inferPrim env [e1, e2] (ops op)
  Lit (LInt _) -> return (nullSubst, typeInt)
  Lit (LBool _) -> return (nullSubst, typeBool)
  Lit (LArray a) -> do
    -- Infer the type of the first element
    (s1, t) <- inferArrayElements env a
    -- Infer the types of the remaining elements and ensure they have the same type
    (s2, _) <- foldM (inferAndUnify env) (s1, t) a
    return (s2, TArray t)

inferArrayElements :: TypeEnv -> [Expr] -> Infer (Subst, Type)
inferArrayElements env [] = do
  tv <- fresh
  return (nullSubst, tv)
inferArrayElements env (e : es) = do
  -- Infer the type of the first element
  (s1, t) <- infer env e
  -- Infer the types of the remaining elements and ensure they have the same type
  (s2, _) <- foldM (inferAndUnify env) (s1, t) es
  return (s2, t)

inferAndUnify :: TypeEnv -> (Subst, Type) -> Expr -> Infer (Subst, Type)
inferAndUnify env (s, t1) e = do
  -- Infer the type of the element
  (s', t2) <- infer env e
  -- Unify the types of the elements
  s'' <- unify (apply s' t1) (apply s' t2)
  return (s'' `compose` s' `compose` s, t1)

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
    inferStep (s, tf) exp = do
      (s', t) <- infer (apply s env) exp
      return (s' `compose` s, tf . TArrow t)

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex) : xs) = do
  ty <- inferExpr env ex
  inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a) = [a]
    fv (TArrow a b) = fv a ++ fv b
    fv (TCon _) = []
    fv (TArray a) = fv a

    normtype (TArrow a b) = TArrow (normtype a) (normtype b)
    normtype (TCon a) = TCon a
    normtype (TVar a) =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"
    normtype (TArray a) = TArray (normtype a)
