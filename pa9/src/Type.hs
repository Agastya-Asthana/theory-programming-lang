{-# LANGUAGE GADTs #-}

module Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type where
  TVar :: TVar -> Type
  TCon :: String -> Type
  TArrow :: Type -> Type -> Type
  TArray :: Type -> Type
  deriving (Show, Eq, Ord)

infixr 9 `TArrow`

data Scheme where
  Forall :: [TVar] -> Type -> Scheme
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"
