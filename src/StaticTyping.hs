{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses #-}

module StaticTyping(
  StaticTypingT,
  getTermIdent,
  getTypeIdent,
  fromTermToType,
  runStaticTypingT
  ) where

import Control.Lens (over, (^.))
import Control.Monad (forM)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (modify, get)
import Control.Unification (UTerm(..), applyBindings, equals)
import Control.Unification.IntVar (IntVar(..))
import Control.Unification.Types (BindingMonad(..), UFailure)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Context
import Grammar
import TypeConstraint
import UtilsOcaml

type StaticTypingT = ExceptT (UFailure Term IntVar) AResult

runStaticTypingT :: StaticTypingT a -> AResult a
runStaticTypingT x = do
  r <- runExceptT x
  case r of
    Left error -> bad $ show error
    Right r' -> return r'

getTermIdent :: OpenTerm -> Maybe Ident
getTermIdent x = case x of
  UTerm t -> case t of
    TermTAlias (TConstr c) -> Just c
    TermTParameterized1 t (TConstr c) -> Just c
    TermTParameterizedN ts (TConstr c) -> Just c
    _ -> Nothing
  UVar _ -> Nothing

getTypeIdent :: Type -> Maybe Ident
getTypeIdent x = case x of
  TAlias (TConstr c) -> Just c
  TParameterized1 t (TConstr c) -> Just c
  TParameterizedN ts (TConstr c) -> Just c
  _ -> Nothing

fromTermToTypeParameters :: TermTypeParameters OpenTerm -> StaticTypingT TypeParameters
fromTermToTypeParameters x = case x of
  TermTParameters2 t1 t2 -> do
    [t1', t2'] <- forM [t1, t2] fromTermToType
    return $ TParameters2 t1' t2'
  TermTParametersN t ts -> do
    t' <- fromTermToType t
    ts' <- fromTermToTypeParameters ts
    return $ TParametersN t' ts'

fromTermToType :: OpenTerm -> StaticTypingT Type
fromTermToType (UTerm x) = case x of
  TermT1Const const -> return $ T1Const const
  TermTIdent ident -> return $ TIdent ident
  TermTAlias t -> return $ TAlias t
  TermTParameterized1 t c -> do
    t' <- fromTermToType t
    return $ TParameterized1 t' c
  TermTParameterizedN ts c -> do
    ts' <- fromTermToTypeParameters ts
    return $ TParameterizedN ts' c
  TermTTuple ts -> do
    ts' <- forM ts fromTermToType
    return $ TTuple ts'
  TermTFunction t1 t2 -> do
    [t1', t2'] <- forM [t1, t2] fromTermToType
    return $ TFunction t1' t2'
fromTermToType x@(UVar (IntVar v)) = do
  v' <- applyBindings x -- We want to fully expand the tree...
  success <- lift $ v' `equals` x
  if success
    then return $ typeNth v
    else fromTermToType v'

instance BindingMonad Term IntVar AResult where
--  lookupVar :: v -> m (Maybe (UTerm t v))
  lookupVar iv = do
    context <- get
    return $ Map.lookup iv (context ^. htypes . assignments)

--  freeVar :: m v
  freeVar = do
    context <- get
    let x = context ^. htypes . nextfresh
    modify $ over (htypes . nextfresh) (+1)
    return $ IntVar x

--  bindVar :: v -> UTerm t v -> m ()
  bindVar v t =
    modify $ over (htypes . assignments) (Map.insert v t)
