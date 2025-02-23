{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Operators where

import Grammar

instance Iso Constant Bool where
  from True = CTrue npos
  from False = CFalse npos
  to =
    \case
      CTrue _ -> True
      CFalse _ -> False
      _ -> undefined

instance Iso Constant Char where
  to =
    \case
      CChar _ c -> c
      _ -> undefined
  from = CChar npos

instance Iso Constant Double where
  to =
    \case
      CDouble _ d -> d
      _ -> undefined
  from = CDouble npos

instance Iso Constant Integer where
  to =
    \case
      CInteger _ i -> i
      _ -> undefined
  from = CInteger npos

instance Iso Constant String where
  to =
    \case
      CString _ c -> c
      _ -> undefined
  from = CString npos

instance Iso Constant a => Iso Expr a where
  to =
    \case
      EConstant _ c -> (to :: Constant -> a) c
      _ -> undefined
  from = EConstant npos . (from :: a -> Constant)

class Iso Expr a =>
      MyApplicative a
  where
  apply1 :: (a -> a) -> Expr -> Expr
  apply1 f = from . f . to
  apply2 :: (a -> a -> a) -> Expr -> Expr -> Expr
  apply2 f x1 x2 = from $ f (to x1) (to x2)

instance MyApplicative Bool

instance MyApplicative Char

instance MyApplicative Double

instance MyApplicative Integer

instance MyApplicative String
