{-# LANGUAGE GADTs #-}

module Syntax where

type Var = String

data Expr where
  Var :: Var -> Expr
  App :: Expr -> Expr -> Expr
  Lam :: Pattern -> Expr -> Expr
  Let :: Var -> Expr -> Expr -> Expr
  Lit :: Lit -> Expr
  If :: Expr -> Expr -> Expr -> Expr
  Fix :: Expr -> Expr
  Op :: Binop -> Expr -> Expr -> Expr
  deriving (Show, Eq, Ord)

data Lit where
  LInt :: Integer -> Lit
  LBool :: Bool -> Lit
  LArray :: [Expr] -> Lit
  deriving (Show, Eq, Ord)

data Pattern where
  PVar :: Var -> Pattern
  PCons :: Var -> Var -> Pattern
  PLit :: Lit -> Pattern
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql | Cons | Concat
  deriving (Eq, Ord, Show)

type Decl = (String, Expr)

data Program where
  Program :: [Decl] -> Expr -> Program
  deriving (Show, Eq)
