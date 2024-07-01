{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Eval
  ( runEval,
    TermEnv,
    emptyTmenv,
    Value (..),
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Data.List (foldl')
import qualified Data.Map as Map
import Syntax

data Value where
  VInt :: Integer -> Value
  VBool :: Bool -> Value
  VClosure :: [(Pattern, Expr)] -> TermEnv -> Value
  VArray :: [Value] -> Value

type TermEnv = Map.Map String Value

type Interpreter t = Identity t

instance MonadFail Identity where
  fail :: String -> Identity a
  fail = error

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show :: Value -> String
  show (VInt n) = show n
  show (VBool n) = show n
  show VClosure {} = "<<closure>>"
  show (VArray a) = show a

-- Add a match function to handle pattern matching
-- When matching against a pattern, you can check:
-- 1. Is the pattern a PVar? -> always match, this is the generic case
-- 2. Is the pattern a PLit? -> match if the argument is equivalent to the literal
-- 3. Is the pattern a (x:xs) structure? -> match if the argument is a non-empty list
-- 4. Otherwise, check another pattern
match :: [(Pattern, Expr)] -> Value -> (Expr, TermEnv)
match [] _ = error "Pattern match failed"
match ((pat, expr) : ps) v = case patMatch pat v of
  Just env -> (expr, env)
  Nothing -> match ps v

-- Function to match a pattern with a value and return a TermEnv if successful
patMatch :: Pattern -> Value -> Maybe TermEnv
patMatch (PVar var) v = Just $ Map.singleton var v
patMatch (PCons x xs) (VArray (v : vs)) = do
  env1 <- patMatch (PVar x) v
  env2 <- patMatch (PVar xs) (VArray vs)
  return $ Map.union env1 env2
patMatch (PLit lit) v
  | lit `checkeq` v = Just Map.empty
  | otherwise = Nothing

checkeq :: Lit -> Value -> Bool
checkeq (LInt litInt) (VInt vInt) = litInt == vInt
checkeq (LBool litBool) (VBool vBool) = litBool == vBool
-- TODO
checkeq _ _ = False

eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
  Lit (LInt k) -> return $ VInt k
  Lit (LBool k) -> return $ VBool k
  Lit (LArray []) -> return $ VArray []
  Lit (LArray xs) -> do
    vals <- mapM (eval env) xs
    return $ VArray vals
  Var x -> do
    let Just v = Map.lookup x env
    return v
  Op Cons e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (v, VArray vs) -> return $ VArray (v : vs)
  Op Concat e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VArray arr1, VArray arr2) -> return $ VArray (arr1 ++ arr2)
  Op op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return $ binop op a' b'
  Lam x body ->
    return (VClosure [(x, body)] env)
  App fun arg -> do
    VClosure pats clo <- eval env fun
    argv <- eval env arg
    let (body, nenv) = match pats argv
    eval (Map.union nenv clo) body
  Let x e body -> do
    e' <- eval env e
    let nenv = Map.insert x e' env
    eval nenv body
  If cond tr fl -> do
    VBool br <- eval env cond
    if br
      then eval env tr
      else eval env fl
  Fix e -> do
    eval env (App e (Fix e))

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

-- TODO-2: Make sure that when you have a new definition for a function, you append the
--         (pattern, body) to the environment instead of overwriting it
runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  let res = runIdentity (eval env ex)
      updatedEnv = case Map.lookup nm env of
        -- function already exists
        Just (VClosure oldVal e) ->
          case res of
            VClosure newPatterns newEnv -> Map.insert nm (VClosure (oldVal ++ newPatterns) newEnv) env
        -- Function doesn't exist, insert the result
        Nothing -> Map.insert nm res env
        Just (VInt _) -> error "int"
        Just (VBool _) -> error "bool"
        Just (VArray _) -> error "array"
   in (res, updatedEnv)
