--
-- This file is responsible for representation of type constraints.
-- This includes: 
-- * utiltities for mapping types to terms
-- * rules of terms' unification
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Terms where

import Control.Lens ((^.), over)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get, modify)
import Control.Unification
  ( Fallible(..)
  , UTerm(..)
  , Unifiable
  , equals
  , fullprune
  , zipMatch
  )
import Control.Unification.IntVar (IntVar(..))
import Control.Unification.Types (BindingMonad(..))
import Data.Char
import qualified Data.Map as Map

import Context
import Grammar

instance Eq a => Eq (UTerm Term a) where
  UTerm t1 == UTerm t2 = t1 == t2
  UVar t1 == UVar t2 = t1 == t2
  _ == _ = False

instance Ord a => Ord (UTerm Term a) where
  UVar t1 <= UVar t2 = t1 <= t2
  UVar _ <= UTerm _ = True
  UTerm _ <= UVar _ = False
  UTerm t1 <= UTerm t2 = t1 <= t2

instance BindingMonad Term IntVar InterpreterT_ where
  lookupVar iv = do
    context <- get
    return $ Map.lookup iv (context ^. htypes . assignments)
  freeVar = do
    context <- get
    let x = context ^. htypes . nextfresh
    modify $ over (htypes . nextfresh) (+ 1)
    return $ IntVar x
  bindVar v t = modify $ over (htypes . assignments) (Map.insert v t)

instance BindingMonad Term IntVar InterpreterT where
  lookupVar iv = lift $ lookupVar iv
  freeVar = lift freeVar
  bindVar v t = lift $ bindVar v t

instance Fallible Term IntVar EError where
  occursFailure t1 t2 =
    ERUnknown npos $ "OccursFailure " ++ show t1 ++ " != " ++ show t2
  mismatchFailure t1 t2 =
    ERUnknown npos $ "MismatchFailure " ++ show t1 ++ " != " ++ show t2

instance Unifiable Term where
  zipMatch x1 x2 =
    case (x1, x2) of
      (TermVariable v1, TermVariable v2)
        | v1 == v2 -> Just . TermVariable $ v1
      (TermTuple ts1, TermTuple ts2)
        | length ts1 == length ts2 ->
          Just $ TermTuple $ zipWith (curry Right) ts1 ts2
      (TermPolymorphic p1 t1, TermPolymorphic p2 t2) ->
        Just $ TermPolymorphic (Right (p1, p2)) (Right (t1, t2))
      (TermFunction t11 t12, TermFunction t21 t22) ->
        Just $ TermFunction (Right (t11, t21)) (Right (t12, t22))
      (TermParams ps1, TermParams ps2)
        | length ps1 == length ps2 ->
          Just $ TermParams $ zipWith (curry Right) ps1 ps2
      _ -> Nothing

instance IsoR Expr OpenTerm where
  toR x =
    case x of
      Variable _ path -> return $ UTerm $ TermVariable (map stripPosition path)
      TIdent _ (ILowercase _ (LowercaseIdent t)) ->
        return . UVar . IntVar $ -1 - which t
      -- Workaround: Why does IntVar v not work?
      -- If a variable of given value existed before, each `freshen` function call 
      -- leads to the duplication of the term-structure, which is not expected
      -- in this usecase (type signatures). We assume that IntVar v is valid if v >= 0.
      -- It is expected that result of this function will be transformed with `freshen`.
      -- Always use `toR x >>= freshen` outside of this module.
      TTuple _ t ts -> UTerm . TermTuple <$> mapM toR (t : ts)
      TPolymorphic _ p t -> do
        p' <- toR p
        t' <- toR t
        return . UTerm $ TermPolymorphic p' t'
      TFunction _ t1 t2 -> do
        t1' <- toR t1
        t2' <- toR t2
        return . UTerm $ TermFunction t1' t2'
      TParams _ ps -> do
        ps' <- mapM toR ps
        return . UTerm $ TermParams ps'
      _ -> bad Nothing $ "Invalid type " ++ show x
  fromR t =
    case t of
      UTerm x ->
        case x of
          TermVariable v -> return $ Variable npos v
          TermTuple ts -> do
            h:tl <- mapM fromR ts
            return $ TTuple npos h tl
          TermPolymorphic p ts -> do
            [p', ts'] <- mapM fromR [p, ts]
            return $ TPolymorphic npos p' ts'
          TermFunction t1 t2 -> do
            [t1', t2'] <- mapM fromR [t1, t2]
            return $ TFunction npos t1' t2'
          TermParams ts -> TParams npos <$> mapM fromR ts
      x@(UVar (IntVar v)) -> do
        v' <- fullprune x -- We want to fully expand the tree...
        success <- v' `equals` x
        if success
          then return $ toTypeIdent v
          else fromR v'

typeComposeFunction :: [Expr] -> Expr
typeComposeFunction = foldr1 (TFunction npos)

termComposeFunction :: [OpenTerm] -> OpenTerm
termComposeFunction = foldr1 (\x acc -> UTerm $ TermFunction x acc)

toTypeIdent :: Int -> Expr
toTypeIdent x =
  let character i = chr (ord 'a' + i)
   in TIdent npos . ILowercase npos . LowercaseIdent
        $ character (x `mod` 26)
            : (if x >= 26
                 then show (x `div` 26)
                 else [])

-- Inverse function to toTypeIdent
which :: String -> Int
which (c:n) =
  (ord c - ord 'a')
    + (if null n
         then 0
         else 26 * Prelude.read n)
which [] = undefined
