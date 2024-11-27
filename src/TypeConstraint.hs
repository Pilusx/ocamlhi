--
-- This file is responsible for representation of type constraints.
-- This includes: 
-- * type constraint data structures
-- * utiltities for mapping types to terms
-- * rules of terms' unification
--

{-# LANGUAGE DeriveTraversable,
             FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses #-}

module TypeConstraint where

-- import Control.Monad.Identity
import Control.Unification (UTerm(..), Unifiable, zipMatch)
import Control.Unification.IntVar (IntVar(..), IntBindingT)
import Data.Maybe (fromJust, isNothing)

import Grammar
import PrintOcaml
import UtilsOcaml

data Term a
    = TermT1Const TypeConst
    | TermTIdent Ident
    | TermTParameterized1 a TypeConstr
    | TermTParameterizedN (TermTypeParameters a) TypeConstr
    | TermTTuple [a]
    | TermTFunction a a
    | TermTAlias TypeConstr
    | TermTBlank
    deriving (Show,
              Ord,
              Eq,
              Foldable,
              Functor,
              Traversable)

data TermTypeParameters a
    = TermTParameters2 a a
    | TermTParametersN a (TermTypeParameters a)
    deriving (Show,
              Ord,
              Eq,
              Foldable,
              Functor,
              Traversable)

-- Unification

instance Ord IntVar where
  IntVar v1 <= IntVar v2 = v1 <= v2

instance Print IntVar where
  prt i e = case e of
    IntVar iv -> doc . showString . show $ iv

instance Eq a => Eq (UTerm Term a) where
  UTerm t1 == UTerm t2 = t1 == t2
  UVar t1 == UVar t2 = t1 == t2

instance Ord a => Ord (UTerm Term a) where
  UVar t1 <= UVar t2 = t1 <= t2
  UVar _ <= UTerm _ = True
  UTerm _ <= UVar _ = False
  UTerm t1 <= UTerm t2 = t1 <= t2

instance (Show a) => Print (UTerm Term a) where
  prt i e = prPrec i 0 (doc . showString . show $ e)

type OpenTerm = UTerm Term IntVar
-- type ClosedTerm = Fix Term
-- type IntBinding = IntBindingT Term Identity

fromTypeToTermTypeParameters :: TypeParameters -> TermTypeParameters OpenTerm
fromTypeToTermTypeParameters x = case x of
  TParameters2 t1 t2 -> TermTParameters2 (fromTypeToTerm t1) (fromTypeToTerm t2)
  TParametersN t1 ts -> TermTParametersN (fromTypeToTerm t1) (fromTypeToTermTypeParameters ts)

getBaseTerms :: OpenTerm -> [OpenTerm]
getBaseTerms (UVar v) = [UVar v]
getBaseTerms (UTerm t) = case t of
  TermTTuple ts -> concatMap getBaseTerms ts
  TermTFunction t1 t2 -> getBaseTerms t1 ++ getBaseTerms t2
  -- TODO
  _ -> [UTerm t]

fromTypeToTerm :: Type -> OpenTerm
fromTypeToTerm x = case x of
  T1Const const -> UTerm $ TermT1Const const
  TIdent ident -> case typeWhich x of
    Nothing -> undefined
    -- Workaround: Why does IntVar v not work?
    -- If a variable of given value existed before, each `freshen` function call 
    -- leads to the duplication of the term-structure, which is not expected
    -- in this usecase (type signatures). We assume that IntVar v is valid if v >= 0.
    -- It is expected that result of this function will be transformed with `freshen`.
    -- Always use `fromTypeToRelabelledTerm` outside of this module.
    Just v -> UVar . IntVar $ -1 - v
  TAlias t ->  UTerm $ TermTAlias t
  TParameterized1 t c -> UTerm $ TermTParameterized1 (fromTypeToTerm t) c
  TParameterizedN ts c -> UTerm $ TermTParameterizedN (fromTypeToTermTypeParameters ts) c
  TTuple ts -> UTerm $ TermTTuple $ map fromTypeToTerm ts
  TFunction t1 t2 -> UTerm $ TermTFunction (fromTypeToTerm t1) (fromTypeToTerm t2)

instance Unifiable TermTypeParameters where
  zipMatch x1 x2 = case (x1, x2) of
    (TermTParameters2 t1x t1y, TermTParameters2 t2x t2y) -> 
      Just $ TermTParameters2 (Right (t1x, t2x)) (Right (t1y, t2y))
    (TermTParametersN t1x t1y, TermTParametersN t2x t2y) -> 
      case zipMatch t1y t2y of
        Just ty -> Just $ TermTParametersN (Right (t1x, t2x)) ty
        _ -> Nothing
    _ -> Nothing

instance Unifiable Term where
  zipMatch x1 x2 = case (x1, x2) of
    (TermT1Const t1, TermT1Const t2) | t1 == t2 -> Just . TermT1Const $ t1
    (TermTIdent t1, TermTIdent t2) | t1 == t2 -> Just . TermTIdent $ t1
    (TermTAlias t1, TermTAlias t2) | t1 == t2 -> Just . TermTAlias $ t1 -- temporary
    (TermTParameterized1 t1 c1, TermTParameterized1 t2 c2) | c1 == c2 -> 
      Just $ TermTParameterized1 (Right (t1, t2)) c1
    (TermTParameterizedN t1 c1, TermTParameterizedN t2 c2) | c1 == c2 ->
      case zipMatch t1 t2 of
        Nothing -> Nothing
        Just t -> Just $ TermTParameterizedN t c1
    (TermTTuple ts1, TermTTuple ts2) | length ts1 == length ts2 -> 
      Just $ TermTTuple $ zipWith (curry Right) ts1 ts2
    (TermTFunction t11 t12, TermTFunction t21 t22) ->
      Just $ TermTFunction (Right (t11, t21)) (Right (t12, t22))
    _ -> Nothing

data TypeConstraint
    = TEquals OpenTerm OpenTerm
    | TSubsumes OpenTerm OpenTerm
  deriving (Eq, Ord, Show)

instance Print TypeConstraint where
  prt i e = case e of
    TSubsumes type1 type2 -> prPrec i 0 (concatD [doc (showString "TSubsumes"), prt 0 type1, doc (showString "]"), prt 0 type2])
    TEquals type1 type2 -> prPrec i 0 (concatD [doc (showString "TEquals"), prt 0 type1, doc (showString "="), prt 0 type2])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showChar '\n'), prt 0 xs]

typeComposeFunction :: [Type] -> Type
typeComposeFunction = foldr1 TFunction

termComposeFunction :: [OpenTerm] -> OpenTerm
termComposeFunction = foldr1 (\x acc -> UTerm $ TermTFunction x acc)

termComposeTuple :: [OpenTerm] -> OpenTerm
termComposeTuple = typeMerge (UTerm . TermTTuple)

fromIntToTerm :: Int -> OpenTerm
fromIntToTerm = UVar . IntVar
